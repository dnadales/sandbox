{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Rule.Delegation.Scheduling where

import           Control.Lens                (makeLenses, (%~), (^.))
import           Control.Monad               (mzero, when)
import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Trans         (lift)
import           Data.Either                 (partitionEithers)
import           Data.Foldable               (traverse_)
import           Data.Function               ((&))
import           Data.List                   (foldl')
import           Data.Set                    (Set, (\\))
import qualified Data.Set                    as Set
import           Test.QuickCheck             (Arbitrary, Gen, arbitrary,
                                              elements, frequency, oneof,
                                              shrink, suchThat)

import           Control.State.TransitionGen
import           Rule.Common

data DCert
  = DCert { _epoch :: Epoch
         , _src    :: VKeyGen
         , _dst    :: VKey
         }
  deriving (Show, Eq)

makeLenses ''DCert

dwho :: DCert -> (VKeyGen, VKey)
dwho c = (c ^. src, c ^. dst)

-- | Delegation scheduling environment
data DSEnv
  = DSEnv
    { _k :: Set VKeyGen
    , _e :: Epoch
    , _s :: Slot
    , _d :: Slot -- TODO: replace this by `SlotCount`
    }
  deriving (Show, Eq)

makeLenses ''DSEnv

-- | Delegation scheduling state
data DSState
  = DSState
    { _sds :: [(Slot, (VKeyGen, VKey))]
    , _eks :: Set (Epoch, VKeyGen)
    }
  deriving (Show, Eq)
makeLenses ''DSState

-- | Some environment to test (put this in some 'Examples' module)
someDSEnv :: DSEnv
someDSEnv
  = DSEnv
    { _k = Set.fromList . fmap (VKeyGen . VKey . Owner) $ [0, 1, 2, 3, 5]
    , _e = Epoch 0
    , _s = Slot 0
    , _d = 10
    }

initialDSState :: DSState
initialDSState
  = DSState
    { _sds = []
    , _eks = Set.empty
    }

--------------------------------------------------------------------------------
-- SDELEG transition system
--------------------------------------------------------------------------------

sdelegAdd :: SigGen DSEnv DSState DCert
sdelegAdd env st = do
  -- Find a @vk_s@, such that (s + d, (vk_s, _)) \notin sds
  let delegatedInSlot = map (fst . snd) $
        filter ((env ^. s + env ^. d ==) . fst) (st ^. sds)
      canDelegateInSlot = foldl' (flip Set.delete) (_k env) delegatedInSlot

  -- If no keys cannot delegate in this slot, we cannot generate a valid
  -- certificate.
  when (null canDelegateInSlot) mzero

  vk_s <- lift . elements . Set.toList $ canDelegateInSlot
  vk_d <- lift arbitrary
  -- TODO: here we might need to be smarter about the way we determine the epoch.
  e_d <- lift $ arbitrary
         `suchThat` (   \e_d -> (e_d, vk_s) `Set.notMember` (st ^. eks)
                     && env ^. e <= e_d
                    )
  let dcert = DCert e_d vk_s vk_d
      nextSt = st & (eks %~ Set.insert (dcert ^. epoch,  dcert ^. src))
                  . (sds %~ ((env ^. s + env ^. d, dwho dcert):))
      -- Replace the line above by nextSt = st and watch it fail!
      --
      -- nextSt = st
  return (dcert, nextSt)

-- | The @SDELEG@ transition rules.
sdeleg :: [SigGen DSEnv DSState DCert]
sdeleg = [sdelegAdd]

sdelegsGen :: Gen (Trace DSState DCert)
sdelegsGen = sigsGen someDSEnv initialDSState sdeleg

instance Arbitrary (Trace DSState DCert) where
  arbitrary = sdelegsGen

  shrink = traceShrink

-- Try this out
--
-- >>> import Test.QuickCheck
-- >>> import Control.Lens
-- >>> tr <- generate sdelegsGen
-- >>> tr ^. traceInitState
-- >>> tr ^. traceTrans
--

--------------------------------------------------------------------------------
-- SDELEGS transition system
--------------------------------------------------------------------------------

sdelegsBase :: SigGen DSEnv DSState [DCert]
sdelegsBase _ st = return ([], st)

sdelegsInd :: SigGen DSEnv DSState [DCert]
sdelegsInd env st = do
  (cs, st') <- apply env st sdelegs
  (c, st'') <- apply env st' sdeleg
  -- TODO: try to make this more efficient.
  return (cs ++ [c], st'')

sdelegs :: [SigGen DSEnv DSState [DCert]]
sdelegs = [sdelegsBase, sdelegsInd]

sdelegssGen :: Gen (Trace DSState [DCert])
sdelegssGen = sigsGen someDSEnv initialDSState sdelegs

--------------------------------------------------------------------------------
-- Adding failures to SDELEG
--------------------------------------------------------------------------------

data SDelegFailure
  = DelegPastEpoch { currEpoch :: Epoch, givenEpoch :: Epoch }
  deriving (Show)

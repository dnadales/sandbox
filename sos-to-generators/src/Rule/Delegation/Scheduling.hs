{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Rule.Delegation.Scheduling where

import           Control.Lens                (makeLenses, (%~), (^.))
import           Data.Either                 (partitionEithers)
import           Data.Foldable               (traverse_)
import           Data.Function               ((&))
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Numeric.Natural             (Natural)
import           QuickCheck.GenT             (GenT, elements, liftGen, listOf)
import           Test.QuickCheck             (Arbitrary, Gen, arbitrary,
                                              suchThat)

import           Control.State.Transition
import           Control.State.TransitionGen

newtype Epoch = Epoch Natural -- TODO: should be a natural number
  deriving (Show, Eq, Ord, Num)

newtype Slot = Slot Natural
  deriving (Show, Eq, Ord, Num)

-- |Representation of the owner of key pair.
newtype Owner = Owner Natural deriving (Show, Eq, Ord)

-- |Signing Key.
newtype SKey = SKey Owner deriving (Show, Eq, Ord)

-- |Verification Key.
newtype VKey = VKey Owner deriving (Show, Eq, Ord)

arbitraryNat :: Gen Natural
arbitraryNat  = fromInteger . abs <$> arbitrary

instance Arbitrary Epoch where
  arbitrary = Epoch <$> arbitraryNat

instance Arbitrary Owner where
  arbitrary = Owner <$> arbitraryNat

instance Arbitrary SKey where
  arbitrary = SKey <$> arbitrary

instance Arbitrary VKey where
  arbitrary = VKey <$> arbitrary

newtype VKeyGen = VKeyGen VKey
  deriving (Show, Eq, Ord)

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
    { k :: Set VKeyGen
    , e :: Epoch
    , s :: Slot
    , d :: Slot -- TODO: replace this by `SlotCount`
    }
  deriving (Show)

-- | Delegation scheduling state
data DSState
  = DSState
    { _sds :: [(Slot, (VKeyGen, VKey))]
    , _eks :: Set (Epoch, VKeyGen)
    }
  deriving (Show)
makeLenses ''DSState

-- | Some environment to test (put this in some 'Examples' module)
someDSEnv :: DSEnv
someDSEnv
  = DSEnv
    { k = Set.fromList . fmap (VKeyGen . VKey . Owner) $ [0, 1, 2, 3, 5]
    , e = Epoch 0
    , s = Slot 0
    , d = 10
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

data SDelegFailure
  = DelegPastEpoch { currEpoch :: Epoch, givenEpoch :: Epoch }
  deriving (Show)

sdelegGen :: SigGen DSEnv DSState DCert SDelegFailure
sdelegGen env st = do
  vk_s <- elements . Set.toList . k $ env
  vk_d <- arbitrary
  -- TODO: here we might need to be smarter about the way we determine the epoch.
  e_d <- arbitrary `suchThat` (\e_d -> (e_d, vk_s) `Set.notMember` (st ^. eks) && e env < e_d)
  let dcert = DCert e_d vk_s vk_d
      nextSt = st & (sds %~ ((s env + d env, dwho dcert):))
                  . (eks %~ Set.insert (dcert ^. epoch,  dcert ^. src))
  return $ Right (dcert, nextSt)

sdelegsGen :: Gen ([(DSState, DCert)], Either SDelegFailure (DCert, DSState))
sdelegsGen = sigsGen someDSEnv initialDSState [sdelegGen]

-- Try this out
--
-- >>> import Test.QuickCheck
-- >>> (stSigs, fStep) <- generate sdelegsGen
--


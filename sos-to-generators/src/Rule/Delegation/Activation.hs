{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
-- | Rules for delegation activation.

module Rule.Delegation.Activation where

import           Prelude                     hiding (any)

import           Control.Arrow               ((***))
import           Control.Lens                (makeLenses, (%~), (^.))
import           Control.Monad               (mzero, unless)
import           Control.Monad.Trans         (lift)
import           Data.Function               ((&))
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Test.QuickCheck             (arbitrary)

import           Control.State.TransitionGen
import           Rule.Common

-- | Delegation activation signal.
type DSig = (Slot, (VKeyGen, VKey))

-- | Pre-filled delegation activation signal.
type PDSig = (Maybe Slot, (Maybe VKeyGen, Maybe VKey))

-- | Delegation activation state.
data DState
  = DState
    { _dms :: Map VKeyGen VKey
    , _dws :: Map VKeyGen Slot
    }
  deriving (Show, Eq)

makeLenses ''DState

initialDState = DState Map.empty Map.empty

adelegAdd :: PreSigGen () DState PDSig DSig
adelegAdd () st (ms, (mvk_s, mvk_d)) = do
  s <- maybe (lift arbitrary) return ms
  vk_s <- maybe (lift arbitrary) return mvk_s
  vk_d <- maybe (lift arbitrary) return mvk_d
  -- If vk_s |-> s_p \in dws, then s_p < s
  --
  -- We might want to come up with some operators for coding this in a way that
  -- looks more like the SOS rules.
  --
  unless (maybe True (< s) (Map.lookup vk_s (st ^. dws))) mzero
  let nextSt = st
             & (dws %~ Map.insert vk_s s)
             . (dms %~ Map.insert vk_s vk_d)
      sig = (s, (vk_s, vk_d))
  return (sig, nextSt)

adelegNoOp :: PreSigGen () DState PDSig DSig
adelegNoOp () st (ms, (mvk_s, mvk_d)) = do
  s <- maybe (lift arbitrary) return ms
  vk_s <- maybe (lift arbitrary) return mvk_s
  vk_d <- maybe (lift arbitrary) return mvk_d
  unless (maybe False (s <=) (Map.lookup vk_s (st ^. dws))) mzero
  let sig = (s, (vk_s, vk_d))
  return (sig, st)

adeleg :: [PreSigGen () DState PDSig DSig]
adeleg = [adelegAdd, adelegNoOp]

adelegsBase :: PreSigGen () DState [PDSig] [DSig]
adelegsBase _ st [] = return ([], st)
adelegsBase _ _ _   = mzero -- We cannot apply the base rule if a non-empty
                            -- list of delegations certificates is expected.

adelegsInd :: PreSigGen () DState [PDSig] [DSig]
adelegsInd _ _ [] = mzero -- We cannot apply the inductive rule if an empty
                          -- list of delegation certificates is expected.
adelegsInd env st (p:pcs) = do
  (ds, st') <- preApply env st pcs adelegs
  (d, st'') <- preApply env st' p adeleg
  return (ds ++ [d], st'')

instance PreSig PDSig DSig where
  any = (Nothing, (Nothing, Nothing))

  asPreSig ps = (Just *** (Just *** Just)) ps

adelegs :: [PreSigGen () DState [PDSig] [DSig]]
adelegs = [adelegsBase, adelegsInd]

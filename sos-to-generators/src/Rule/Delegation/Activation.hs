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
             & (dms %~ Map.insert vk_s vk_d)
             & (dws %~ Map.insert vk_s s)
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

adelegsInd :: PreSigGen () DState [PDSig] [DSig]
adelegsInd env st pcs = do
  (ds, st') <- preApply env st (gamma pcs) adelegs
  (d, st'') <- preApply env st (dPre pcs) adeleg
  return (ds ++ [d], st'')
  where
    gamma []  = any @[PDSig] @[DSig]
    gamma pcs = init pcs

    dPre []  = any @PDSig @DSig
    dPre pcs = last pcs

instance PreSig PDSig DSig where
  any = (Nothing, (Nothing, Nothing))

  asPreSig ps = (Just *** (Just *** Just)) ps

adelegs :: [PreSigGen () DState [PDSig] [DSig]]
adelegs = [adelegsBase]

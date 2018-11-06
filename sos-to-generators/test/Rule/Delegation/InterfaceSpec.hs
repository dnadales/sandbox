{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module Rule.Delegation.InterfaceSpec where

import           Control.Lens                (makeLenses, (%~), (.~), (^.))
import           Control.Monad.Trans         (lift)
import           Data.Function               ((&))
import           Test.Hspec                  (Spec, it, pending)
import           Test.QuickCheck             (Arbitrary, Gen, arbitrary,
                                              property, shrink)

import           Control.State.TransitionGen
import           Rule.Common
import           Rule.Delegation.Interface
import           Rule.Delegation.Scheduling

data DummyBlock
  = DummyBlock
    { _slot  :: Slot
    , _certs :: [DCert]
    }
  deriving (Show, Eq)

makeLenses ''DummyBlock

delegationMapCorrectlyUpdated
  :: Trace (DSEnv, DIState) DummyBlock
  -> Bool
delegationMapCorrectlyUpdated = undefined

dummyBlock :: SigGen () (DSEnv, DIState) DummyBlock
dummyBlock () (dsEnv, diState) = do
  n <- lift arbitraryNat
  let
    nextSlot = slotInc n (dsEnv ^. s)
    nextEnv = dsEnv & s .~ nextSlot
  (cs, nextDiState) <- apply nextEnv diState deleg
  let nextBlock = DummyBlock nextSlot cs
  return (nextBlock, (nextEnv, nextDiState))

dummyBlocksGen :: Gen (Trace (DSEnv, DIState) DummyBlock)
dummyBlocksGen = sigsGen () (someDSEnv, initialDIState) [dummyBlock]

instance Arbitrary (Trace (DSEnv, DIState) DummyBlock) where
  arbitrary = dummyBlocksGen

spec :: Spec
spec = it "The delegation map is correctly updated" $
       property delegationMapCorrectlyUpdated

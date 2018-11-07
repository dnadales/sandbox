{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module Rule.Delegation.InterfaceSpec where

import           Control.Arrow               ((&&&))
import           Control.Lens                (makeLenses, (%~), (.~), (^.))
import           Control.Monad.Trans         (lift)
import           Data.Function               ((&))
import qualified Data.Map                    as Map
import           Test.Hspec                  (Spec, it, pending)
import           Test.QuickCheck             (Arbitrary, Gen, Property,
                                              arbitrary, property, shrink,
                                              suchThat, (===))

import           Control.State.TransitionGen
import           Rule.Common
import           Rule.Delegation.Activation
import           Rule.Delegation.Interface
import           Rule.Delegation.Scheduling

data DummyBlock
  = DummyBlock
    { _slot  :: Slot
    , _certs :: [DCert]
    }
  deriving (Show, Eq)

makeLenses ''DummyBlock

dcersAreTriggered
  :: Trace (DSEnv, DIState) DummyBlock
  -> Property
dcersAreTriggered tr =
  lastDms === expectedDms
  where
    -- | Delegation map in the last state.
    lastDms = lastSt ^. activation . dms

    -- | The states in the trace are non-empty (it contains at least the
    -- initial state). We take the last state which is in the head.
    (lastEnv, lastSt) = head $ traceStates tr

    -- | The expected delegation map can be constructed by taking all the
    -- certificates and applying them, starting with the empty map. We use
    -- 'reverse' since the signals come in a 'newest-first' order, and we want
    -- to apply the oldest certificate first. Note that 'Map.fromList xs' will
    -- produce a map equivalent to inserting all the elements in @xs@ in the
    -- order in which they appear. So if a key delegates more than once, only
    -- the newest certificate will appear in the map.
    expectedDms = Map.fromList (reverse (fmap (_src &&& _dst) allActiveCerts))

    allActiveCerts :: [DCert]
    allActiveCerts = concatMap _certs activeBlocks

    -- | We keep all the blocks whose certificates should be active given the
    -- current slot. The current slot can be found in the last state in the
    -- trace (see 'lastSlot').
    activeBlocks :: [DummyBlock]
    activeBlocks = filter (\b -> b ^. slot <= activationSlot) (traceSigs tr)

    activationSlot :: Slot
    activationSlot = lastSlot - ((lastEnv ^. d) `min` lastSlot)

    lastSlot = lastEnv ^. s

-- | This corresponds to a state-transition rule where blocks with increasing
-- slot-numbers are produced.
dummyBlock :: SigGen () (DSEnv, DIState) DummyBlock
dummyBlock () (dsEnv, diState) = do
  n <- lift $ arbitraryNat `suchThat` (0 <)
  let
    nextSlot = slotInc n (dsEnv ^. s)
    nextEnv = dsEnv & s .~ nextSlot
  (cs, nextDiState) <- apply nextEnv diState deleg
  let nextBlock = DummyBlock nextSlot cs
  return (nextBlock, (nextEnv, nextDiState))

dummyBlocksGen :: Gen (Trace (DSEnv, DIState) DummyBlock)
dummyBlocksGen = sigsGen () (someDSEnv {_d = 3}, initialDIState) [dummyBlock]

instance Arbitrary (Trace (DSEnv, DIState) DummyBlock) where
  arbitrary = dummyBlocksGen

  shrink = traceShrink

spec :: Spec
spec = it "The delegation map is correctly updated" $
       property dcersAreTriggered

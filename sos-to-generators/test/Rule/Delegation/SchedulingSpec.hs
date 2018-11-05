module Rule.Delegation.SchedulingSpec where

import           Control.Arrow               ((&&&))
import           Data.List.Unique            (allUnique)
import           Test.Hspec                  (Spec, it, pending)
import           Test.QuickCheck             (forAll)

import           Control.State.TransitionGen
import           Rule.Delegation.Scheduling

spec :: Spec
spec =
  it "One-delegation per-key per epoch" $
  forAll sdelegsGen oneDelegPerKeyPerEpoch


oneDelegPerKeyPerEpoch
  :: ([(DSState, DCert)], Either SDelegFailure (DCert, DSState))
  -> Bool
oneDelegPerKeyPerEpoch (_, Left _)  = False
oneDelegPerKeyPerEpoch (stSigs, Right (c, _)) =
  allUnique $ fmap (_epoch &&& _src) (c:(sigs stSigs))

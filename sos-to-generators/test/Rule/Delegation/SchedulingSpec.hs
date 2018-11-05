module Rule.Delegation.SchedulingSpec where

import           Control.Arrow               ((&&&))
import           Control.Lens                (makeLenses, (%~), (^.))
import           Data.List.Unique            (allUnique)
import           Test.Hspec                  (Spec, it, pending)
import           Test.QuickCheck             (property)

import           Control.State.TransitionGen
import           Rule.Delegation.Scheduling

spec :: Spec
spec =
  it "One-delegation per-key per epoch" $
  property oneDelegPerKeyPerEpoch


oneDelegPerKeyPerEpoch
  :: Trace DSState DCert SDelegFailure
  -> Bool
oneDelegPerKeyPerEpoch tr = case tr ^. traceHead of
  Left _       -> error "TODO: think about what's the meaning of this ..."
  Right (c, _) -> allUnique $ fmap (_epoch &&& _src) (c:(traceSigs tr))

module Main where

import           Control.Monad.IOSim                 (SimEventType (EventSay),
                                                      SimTrace, ppTrace,
                                                      runSimTrace, traceEvents)
import           Examples.Control.Monad.IOSim.Basics (sayHello)
import           Test.QuickCheck                     (Property, (===))
import           Test.Tasty                          (TestTree, defaultMain,
                                                      testGroup)
import           Test.Tasty.QuickCheck               (counterexample, property,
                                                      testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Examples" [testProperty "Says hello" prop_saysHello]

-- | For the 'sayHello' example we test that the "Hello World" string is
-- actually output.
--
-- Modify either the expected string or the string that 'sayHello' passes to
-- 'say' to experiment with test failures.
prop_saysHello :: Property
prop_saysHello = lookForExactly1HelloWorld simEvents
  where
    trace :: SimTrace ()
    trace = runSimTrace sayHello

    simEvents = fmap ( \(_time, _threadId, _mThreadLabel, simEventType) -> simEventType)
                     $ traceEvents trace
    -- traceEvents :: SimTrace a -> [(Time, ThreadId, Maybe ThreadLabel, SimEventType)]
    -- data SimEventType
    --   = EventSimStart      ScheduleControl
    --   | EventSay  String


    lookForExactly1HelloWorld []                           = failProp
    lookForExactly1HelloWorld (EventSay "Hello World": xs) = noMoreEventSay xs
    lookForExactly1HelloWorld (_:xs)                       = lookForExactly1HelloWorld xs

    noMoreEventSay []                          = property True
    noMoreEventSay (EventSay "Hello World": _) = failProp
    noMoreEventSay (_:xs)                      = noMoreEventSay xs


    failProp = counterexample (ppTrace trace) False



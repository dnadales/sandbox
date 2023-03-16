module Main (main) where


import           Control.Monad.Class.MonadSay        (MonadSay, say)
import           Control.Monad.IOSim                 (IOSim, runSimTrace,
                                                      selectTraceEventsDynamic,
                                                      traceM)
import           Control.Tracer                      (Tracer (..), emit)
import           Data.Data                           (Typeable)
import           Examples.Control.Monad.IOSim.Basics (MyTrace, run, sayHello,
                                                      traceAValue)



sayTracer :: (Show a, MonadSay m) => Tracer m a
sayTracer = Tracer $ emit $ say . show

ioSimTracer :: (Typeable a) => Tracer (IOSim s) a
ioSimTracer = Tracer $ emit traceM

-- selectTraceEventsDynamic :: forall a b. Typeable b => SimTrace a -> [b]



main :: IO ()
main = do
  putStrLn "Example run with simulated IO: "
  putStrLn $ run sayHello
  putStrLn "Example run with actual IO: "
  sayHello
  putStrLn "Running our custom tracer with simulated IO and sayTracer: "
  putStrLn $ run $ traceAValue sayTracer 10
  putStrLn "Running our custom tracer with simulated IO and ioSimTracer: "
  putStrLn $ run $ traceAValue ioSimTracer 10
  putStrLn "Running our custom tracer with simulated IO and sayTracer <> ioSimTracer: "
  putStrLn $ run $ traceAValue (sayTracer <> ioSimTracer) 10
  putStrLn "Running our custom tracer with simulated IO and selecting only the 'MyTrace' events: "
  let
    myTrace :: [MyTrace]
    myTrace = selectTraceEventsDynamic $ runSimTrace $ traceAValue ioSimTracer 10
  putStrLn $ show myTrace
  putStrLn "Running our custom tracer with actual IO"
  traceAValue sayTracer 10

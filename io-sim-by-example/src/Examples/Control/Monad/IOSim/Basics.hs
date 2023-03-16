{-# LANGUAGE RankNTypes #-}
-- | The examples in this module were taken from:
--
-- https://github.com/well-typed/iosim-zurihac-2022/
module Examples.Control.Monad.IOSim.Basics where

import           Control.Concurrent.Class.MonadSTM.TMVar (newEmptyTMVarIO,
                                                          putTMVar, takeTMVar)
import           Control.Monad.Class.MonadFork           (MonadFork (..),
                                                          labelThisThread)
import           Control.Monad.Class.MonadSTM            (MonadSTM (..))
import           Control.Monad.Class.MonadSay            (MonadSay (..))
import           Control.Monad.Class.MonadTimer          (DiffTime,
                                                          MonadDelay (..))
import           Control.Monad.IOSim                     (IOSim, SimTrace,
                                                          ppTrace, runSimTrace)
import           Control.Tracer                          (Tracer, traceWith)

-- | Running an example will produce a trace, which we will convert to 'String'.
run
  :: forall a
   . Show a
  => (forall s . IOSim s a) -> String
run e = ppTrace $ runSimTrace e
  -- ppTrace     :: (a -> String) -> (b -> String) -> Trace a b -> String
  -- runSimTrace :: forall a. (forall s. IOSim s a) -> Trace a

-----------------------------------------------------------------------------------------
-- Examples
-----------------------------------------------------------------------------------------

-- | Just say hello world.
--
sayHello :: MonadSay m => m ()
sayHello = say "Hello World"

-- | Add a custom tracer.
traceAValue :: Monad m => Tracer m MyTrace -> Int -> m ()
traceAValue tr = traceWith tr . TraceValue

data MyTrace = TraceValue Int
  deriving (Eq, Show)

-- | Race on a transactional mutable variable.
raceOnTMVar :: (MonadFork m, MonadSTM m, MonadDelay m, MonadSay m) => m ()
raceOnTMVar = do
  result <- newEmptyTMVarIO

  _ <- forkIO $ do
    threadDelay 5
    say "Calculated result!"
    atomically (putTMVar result (42 :: Int))

  _ <- forkIO $ do
    threadDelay 5
    say "Calculated result!"
    atomically (putTMVar result 24)

  say "Waiting..."
  value <- atomically $ takeTMVar result
  say ("The answer is: " ++ show value)

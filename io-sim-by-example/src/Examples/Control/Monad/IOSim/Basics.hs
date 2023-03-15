{-# LANGUAGE RankNTypes #-}
-- | The examples in this module were taken from:
--
-- https://github.com/well-typed/iosim-zurihac-2022/
module Examples.Control.Monad.IOSim.Basics where

import           Control.Monad.Class.MonadSay (MonadSay (..))
import           Control.Monad.IOSim          (IOSim, SimTrace, ppTrace,
                                               runSimTrace)

-- Very simple example
--
sayHello :: MonadSay m => m ()
sayHello = do
  say "Hello World!"

-- | Running an example will produce a trace, which we will convert to 'String'.
run
  :: forall a
   . Show a
  => (forall s . IOSim s a) -> String
run e = ppTrace $ runSimTrace e
  -- ppTrace     :: (a -> String) -> (b -> String) -> Trace a b -> String
  -- runSimTrace :: forall a. (forall s. IOSim s a) -> Trace a

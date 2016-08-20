-- | Monadic counters.

module Counter where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

newtype Counter = MkCounter {cValue :: Int}
                deriving (Show)

-- | 'inc c n' increments the counter by 'n' units.
inc :: Counter -> Int -> Counter
inc (MkCounter c) n = MkCounter (c + n)

-- | The computation we want to run.
mComputation :: Counter
mComputation = inc (inc (inc (inc (inc (MkCounter 0) 3) 3) 3) 5) 5

-- | Run and show the computation.
runComputation :: IO ()
runComputation = putStrLn $ show mComputation

-- * The monadic counter.

-- | CounterS is a monad.
type CounterS = State Counter

-- | Increment the counter by 'n' units.
incS :: Int-> CounterS ()
incS n = modify (\c -> inc c n)

-- | The computation we want to run, with the state monad.
mComputationS :: CounterS ()
mComputationS = do
  incS 3
  incS 3
  incS 3
  incS 5
  incS 5

runComputationS :: IO ()
runComputationS = putStrLn $ show . snd $ runState mComputationS (MkCounter 0)

-- * Introducing an environment.
type CounterRS = ReaderT Int CounterS

-- | Increment the counter by the amount of units specified by the environment.
incR :: CounterRS ()
incR = ask >>= lift . incS

-- | The computation we want to run, using reader and state monads.
mComputationRS :: CounterRS ()
mComputationRS = do
  local (const 3) $ do
    incR
    incR
    incR
    local (const 5) $ do
      incR
      incR

runComputationRS :: IO ()
runComputationRS = putStrLn $ show . snd $ runState compWithEnv (MkCounter 0)
  where compWithEnv = runReaderT mComputationRS 15

-- * Adding logging.
type CounterWRS = WriterT [Int] CounterRS

incW :: CounterWRS ()
incW = lift incR >> get >>= tell . (:[]) . cValue

mComputationWRS :: CounterWRS ()
mComputationWRS = do
  local (const 3) $ do
    incW
    incW
    incW
    local (const 5) $ do
      incW
      incW

runComputationWRS :: IO ()
runComputationWRS = putStrLn $ show $ runState compWithEnv (MkCounter 0)
  where compWithEnv = runReaderT compWithLogs 15
        compWithLogs = (runWriterT mComputationWRS)

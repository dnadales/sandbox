-- | A simple counter.

module Counter where

import           Control.Monad.Reader
import           Control.Monad.State.Lazy

newtype Counter = MkCounter Int
  deriving (Show)

-- | `inc c n` increments the counter by `n` units.
inc :: Counter -> Int -> Counter
inc (MkCounter c) n = MkCounter (c + n)

manipCounter :: Counter -> Counter
manipCounter c = inc (inc (inc (inc (inc c 3) 3) 3) 5) 5

incS :: Int -> State Counter ()
incS n = modify (\c -> inc c n)

incR :: Reader Int (State Counter ())
-- Types are getting hairy!
incR = asks incS

-- Then, can we write what we where longin for?
manipCounterM :: Reader Int (State Counter ())
manipCounterM =
  local (const 3) (
  incR >>
  incR >>
  incR >> local (const 5) (
      incR >>
      incR
      )
  )

manipCounterM' :: Reader Int (State Counter ())
manipCounterM' = do
  local (const 3) $
    do incR
       incR
       incR
       local (const 5) $
         do incR
            incR

runCounter :: Counter
runCounter = manipCounter (MkCounter 0)

-- | What is happening here?
--
-- If you look at the definition of `incR` we see that we are changing State
-- monad, but the state monad never gets executed! Are monad transformers the solution?
manipCounterM2 :: Reader Int (State Counter ())
manipCounterM2 = incR >> incR

runCounterM :: ((), Counter)
runCounterM = runState (runReader manipCounterM2 1) (MkCounter 0)

showCounter :: IO ()
showCounter = putStrLn $ show runCounter

showCounterM :: IO ()
showCounterM = putStrLn $ show $ snd runCounterM

-- | Results are not equal!!!


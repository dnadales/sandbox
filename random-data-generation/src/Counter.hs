-- | A monadic counter.

module Counter where

import           Control.Monad.Reader
import           Control.Monad.State.Lazy

newtype Counter = MkCounter Int
  deriving (Show)

-- | `inc c n` increments the counter by `n` units.
inc :: Counter -> Int -> Counter
inc (MkCounter c) n = MkCounter (c + n)

incS :: Int -> State Counter ()
incS n = modify (\c -> inc c n)

incS' :: State Counter Int -> State Counter ()
incS' stc = stc >>= incS

-- What if we would have defined:
incR :: ReaderT Int (State Counter) ()
incR = mapReaderT incS' ask

incTwo :: ReaderT Int (State Counter) ()
incTwo = incR >> incR
-- What did just happen? How is different from our wrong implementation? Welll,
-- in incs we're actually calling the >>= operation of the State monad that is
-- performing the increments!

manipCounter :: Counter -> Counter
manipCounter c = inc (inc (inc (inc (inc c 3) 3) 3) 5) 5

manipCounterM :: ReaderT Int (State Counter) ()
manipCounterM =
  local (const 3) (
  incR >> incR >> incR >>
    local (const 5) (
      incR >> incR
    )
  )

runCounterM :: ReaderT Int (State Counter) () -> IO ()
runCounterM rsc = putStrLn $ show $ snd $ runState (runReaderT rsc 1) (MkCounter 0)

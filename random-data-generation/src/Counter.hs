-- | A monadic counter.

module Counter where

import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Control.Monad.Writer

newtype Counter = MkCounter {cValue :: Int}
  deriving (Show)

-- | `inc c n` increments the counter by `n` units.
inc :: Counter -> Int -> Counter
inc (MkCounter c) n = MkCounter (c + n)

incS :: Int -> State Counter ()
incS n = modify (\c -> inc c n)

-- We need to define operations to retrieve the value of the counter:
getS :: State Counter Int
getS = get >>= (return . cValue)

-- * Introducing the reader monad
incR :: ReaderT Int (State Counter) ()
incR = ask >>= lift . incS

incTwo :: ReaderT Int (State Counter) ()
incTwo = incR >> incR
-- What did just happen? How is different from our wrong implementation? Well,
-- the laws of 'lift' dictate that when composing monads via lift, the '>>='
-- operator from the inner monad will be called.
--
-- > lift m >>= (lift . f) = lift (m >>= f)
--
-- And we have:
--
-- > incR >> incR
-- > = {def. incR}
-- > (ask >>= lift . incS) >> (ask >>= lift . incS)
-- > = {associativity}
-- > ask >>= (\n -> (lift . incS) n  >> (ask >>= lift . incS))
-- > = {ask will have the same result, hence we can remove it.}
-- > ask >>= (\n -> (lift . incS) n  >>= lift . incS))
-- > = {def. composition}
-- > ask >>= (\n -> lift  (incS n)  >>= lift . incS))
-- > = {lift laws}
-- > ask >>= (\n -> lift ((incS n)  >>= incS))
--
-- Here we see that the '>>=' operator gets called.

-- | Reader version for 'getS':
getR :: ReaderT Int (State Counter) Int
getR = lift getS

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

-- * Introducing the writer monad.
-- What if we wanted to trace the history of the counter?
-- First, let's sort out the mess with types:
type CounterRS = ReaderT Int (State Counter)

-- The instances say...
-- MonadWriter [Int] (State Counter) => MonadWriter [Int] (ReaderT Int (State Counter))

incW :: WriterT [Int] CounterRS ()
incW = lift incR >> lift getR >>= \x -> tell [x]

incTwoRSW :: WriterT [Int] CounterRS ()
incTwoRSW = incW >> incW

-- class Monad m => MonadReader r m | m -> r where
--
--    local :: (r -> r) -- ^ The function to modify the environment.
--          -> m a      -- ^ @Reader@ to run in the modified environment.
--          -> m a

-- Why we can use the local function from reader in the writer monad?
localR :: WriterT [Int] CounterRS ()
localR = local (const 15) incW

-- Because we get this instance for free!
-- (Monoid [Int], MonadReader Int CounterRS) => MonadReader Int (WriterT [Int] CounterRS)

manipCounterWRS :: WriterT [Int] CounterRS ()
manipCounterWRS = do
  local (const 3) $ do
    incW
    incW
    incW
    local (const 5) $ do
      incW
      incW

runCounterWRS :: WriterT [Int] CounterRS () -> IO ()
runCounterWRS counter = putStrLn $ show  $ runState (runReaderT rsc 1) (MkCounter 0)
  where rsc = runWriterT counter

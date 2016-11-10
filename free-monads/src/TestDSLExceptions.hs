{-# LANGUAGE DeriveFunctor #-}
-- | What if we want to stop the interpreter as soon as an error occurs?

module TestDSLExceptions where

import           Control.Monad.Free
import           Control.Monad.State.Lazy   hiding (put)
import           Control.Monad.Trans.Either


type Elem = String

-- | A simple language to put elements into a bag, and count the number of
-- them.
--
-- Notice that in principle exceptions should not be part of the domain
-- language, since putting an element on a bag should always succeed (we could
-- assume we drop elements if the bag overflows), and counting the elements
-- should always succeed as well (if an element does not exist its count is 0).
data TestF r = Put Elem r | Count Elem (Int -> r)
  deriving Functor

type TestM = Free TestF

put :: Elem -> TestM ()
put e = liftF $ Put e ()

count :: Elem -> TestM Int
count e = liftF $ Count e id

-- | Sample programs:
test0 :: TestM [(Elem, Int)]
test0 = do
  put "Apple"
  put "Apple"
  put "Pear"
  nApples <- count "Apple"
  nPears <- count "Pear"
  nBananas <- count "Banana"
  return [("Apple", nApples), ("Pears", nPears), ("Bananas", nBananas)]

-- | An interpreter that uses a list of elements:
interpS :: TestF r ->  State [Elem] r
interpS (Put e next) = do
  modify (e:)
  return next
interpS (Count e fNext) = do
  nElems <- gets (length . filter (== e))
  return (fNext nElems)

runState :: IO ()
runState = putStrLn $ show $ result
  where result = evalState (foldFree interpS test0) []

-- | Now what if we want simulate a situation where a fatal error is thrown in
-- an IO interpreter?

interpE :: TestF r -> EitherT String IO r
interpE (Put "Pinneaple" next) =
  left "you cannot put pinneaples here!"

interpE (Put e next) = do
  lift $ putStrLn $ "Putting " ++ e
  return next

interpE (Count e fNext) =
  right (fNext 42)


test1 :: TestM [(Elem, Int)]
test1 = do
  put "Apple"
  put "Orange"
  put "Pinneaple"
  nApples <- count "Apple"
  nPears <- count "Pear"
  nBananas <- count "Banana"
  return [("Apple", nApples), ("Pears", nPears), ("Bananas", nBananas)]

runError :: (Show r) => TestM r -> IO ()
runError test = do
  r <- runEitherT (foldFree interpE test)
  putStrLn (show r)

-- | Finally let's mix State, IO, and error-handling.
type IOErrorT = EitherT String IO

interp :: TestF r -> StateT [Elem] IOErrorT r
interp t@(Put e next) = do
  modify (e:)
  lift $ interpE t
interp t@(Count e fNext) = do
  nElems <- gets (length . filter (== e))
  lift $ interpE t
  return (fNext nElems)

run :: (Show r) => TestM r -> IO ()
run test = do
  r <- runEitherT $ evalStateT (foldFree interp test) []
  putStrLn (show r)

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data Foo = Foo { bar :: Int, baz :: Char } deriving (Eq, Show)

newtype Trace = Trace [Foo] deriving (Eq, Show)

randomFoo :: Gen Foo
randomFoo = do
  n <- Gen.filter (\x -> (42 <= x) && (x<100)) $ Gen.integral (Range.linear 1 1000)
  c <- Gen.filter (`elem` ("?!" :: String)) Gen.ascii
  return $ Foo n c

randomFooND :: Gen Foo
randomFooND = do
  n <- intInRange
  c <- charInRange
  return $ Foo n c
  where
    intInRange = do
      n <- Gen.integral (Range.constant 0 100000)
      if (567 <= n) && (n<568)
        then return n
        else intInRange
    charInRange = do
      c <- Gen.ascii
      if c `elem` ("?!" :: String)
        then return c
        else charInRange


anyFoo :: Gen Foo
anyFoo = do
  n <- Gen.integral (Range.linear 1 1000)
  c <- Gen.ascii
  return $ Foo n c

smallFoo :: Gen Foo
smallFoo = do
  n <- Gen.integral (Range.constant 1 3)
  c <- Gen.element "!abcd"
  return $ Foo n c


randomTrace :: Gen Trace
randomTrace = Trace <$> Gen.list (Range.linear 1 1000) randomFoo

randomTraceND :: Gen Trace
randomTraceND = Trace <$> Gen.list (Range.linear 1 1000) randomFooND

randomTraceAnyFoo :: Gen Trace
randomTraceAnyFoo = Trace <$> Gen.list (Range.linear 1 1000) anyFoo

foosAreWrong :: Foo -> Bool
foosAreWrong (Foo n c) = 200 < n && c == 'a'

noTracesAreValid :: Property
noTracesAreValid = property $
  -- Using 'randomTraceND' will probably eat up all the system's ram. Might
  -- have to do with the huge number of shrinks that are generated!
  forAll randomTraceND >>= \(Trace tr) -> assert (any foosAreWrong tr)

--------------------------------------------------------------------------------
-- Some record
--
-- Example taken from: https://www.youtube.com/watch?v=AIv_9T0xKEo
--------------------------------------------------------------------------------

data SomeRecord
  = SomeRecord
  { someInt :: Int
  , someList :: Trace
  } deriving (Show, Eq)

arbitraryRecord :: Gen SomeRecord
arbitraryRecord = do
  i <- Gen.integral (Range.linear 1 1000)
  xs <- randomTraceAnyFoo
  return $ SomeRecord i xs
  -- where
  --   arbitraryPair = (,) <$> arbitraryInt <*> arbitraryString
  --   arbitraryInt = Gen.integral (Range.linear 1 1000)
  --   arbitraryString = Gen.string (Range.linear 1 100) Gen.ascii

recordsAreEqual :: Property
recordsAreEqual = property $ do
  r0 <- forAll arbitraryRecord
  r1 <- forAll arbitraryRecord
  r0 === r1

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

stringsAreWrong :: Property
stringsAreWrong = property $ do
  xs <- forAll arbitraryString
  xs === "hello"
  where
    arbitraryString = Gen.list (Range.constant 1 4) (Gen.element "abcdefghijklmn")

aSpecificChar :: Gen Char
aSpecificChar = do
  c <- Gen.element "abxcd"
  if c `elem` ("x" :: String)
    then return c
    else aSpecificChar

aFilteredchar :: Gen Char
aFilteredchar =
  Gen.filter (`elem` ("x" :: String)) (Gen.element "ax")

charMustNotBeX :: Property
charMustNotBeX = property $ do
  c <- forAll aFilteredchar
  c /== 'x'

charsMustNotBeX :: Property
charsMustNotBeX = property $ do
  c0 <- forAll aFilteredchar
  c1 <- forAll aFilteredchar
  assert (c0 /= 'x' || c1 /= 'x')

--------------------------------------------------------------------------------
-- Try to generate non-minimal counter examples
--------------------------------------------------------------------------------

notLargeOrBothNotEmpty :: Property
notLargeOrBothNotEmpty = property $ do
  xs <- forAll randomIntLists
  ys <- forAll randomIntLists
  assert $ length xs < 10 && (xs /= [] && ys /=[])
  where
    randomIntLists = Gen.frequency
      [ (1, Gen.list (Range.constant 0 1) randomInt)
      , (10, Gen.list (Range.constant 1 100000) randomInt)
      ]
    randomInt = Gen.integral (Range.constant 1 100)

-- I don't know whether this is a good example either, since hedgehog will
-- shrink towards the first element in the list.

main :: IO Bool
main = checkParallel $ Group "Test.Example"
  [("Special pair", prop_list)]
--  [("Produce a minimal counter-example", notLargeOrBothNotEmpty)] -- This is a test that sometimes eats up all of HH memory
-- main :: IO Bool
-- main = checkParallel $ Group "Test.Example"
--   [ ("No traces are valid", noTracesAreValid)
--   , ("Records are equal", recordsAreEqual)
--   , ("Strings are wrong", stringsAreWrong)
--   , ("Chars must not be x", charsMustNotBeX)
--   , ("Produce a minimal counter-example", notLargeOrBothNotEmpty)
--   ]

--------------------------------------------------------------------------------
-- Example of dependent shrinking, to compare against quick-check
--------------------------------------------------------------------------------

-- prop_specialPair :: Property
-- prop_specialPair = property $ do
--   (_, xs) <- forAll specialPair
--   case xs of
--     x:_ -> x /== 7
--     _   -> success

data List = List Int [Int]
  deriving (Show)

listGen :: Gen List
listGen = do
  n <- genInt
  xs <- Gen.list (Range.singleton n) genInt
  pure $ List n xs
  where
    genInt = Gen.integral (Range.linear 0 10)

prop_list :: Property
prop_list = property $ do
  List _ xs <- forAll listGen
  assert $ length xs < 5

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

main :: IO Bool
main = checkParallel $ Group "Test.Example" [
      ("No traces are valid", noTracesAreValid)
    ]

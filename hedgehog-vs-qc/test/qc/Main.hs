{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Test.QuickCheck


--------------------------------------------------------------------------------
-- Evens are even
--------------------------------------------------------------------------------

-- The `evenAreEven` property is just to show that the argument given here
--
-- - https://hypothesis.works/articles/integrated-shrinking/
--
-- on why the quickcheck approach will give an invalid counter example doesn't
-- seem to be correct. Here we will never have 1 as a counter example.
--

newtype EvenInt = EvenInt Int deriving (Eq, Show)

instance Arbitrary EvenInt where
  arbitrary = EvenInt . (*2) <$> arbitrary

evensAreEven :: EvenInt -> Bool
evensAreEven (EvenInt i) =
  i `mod` 2 == 0 && i <= 4

--------------------------------------------------------------------------------
-- How will QuickCheck perform when we need several tries to generate a value?
--------------------------------------------------------------------------------

data Foo = Foo { bar :: Int, baz :: Char } deriving (Eq, Show)

instance Arbitrary Foo where
  arbitrary = do
    n <- arbitrary `suchThat` (\x -> (42 <= x) && (x<43))
    c <- arbitrary `suchThat` (`elem` "?!")
    return $ Foo n c

randomFooND :: Gen Foo
randomFooND = do
  n <- intInRange
  c <- charInRange
  -- This will also cause QuickCheck to struggle a lot with finding an element
  -- that satisfies the predicate!
  return $ Foo n c
  where
    intInRange = do
      n <- arbitrary
      if n < 0  -- (-1 <= n) && (n<2147483648)
        then return n
        else intInRange
    charInRange = do
      c <- arbitrary
      if c `elem` ("?!" :: String)
        then return c
        else charInRange

foosAreWrong :: Foo -> Bool
foosAreWrong (Foo n c) = 200 < n && c == 'a'

newtype Trace = Trace [Foo] deriving (Eq, Show)

instance Arbitrary Trace where
  arbitrary = Trace <$> listOf1 arbitrary

randomTraceND :: Gen Trace
randomTraceND = Trace <$> listOf1 randomFooND

noTracesAreValid :: Trace -> Bool
noTracesAreValid (Trace xs) = any foosAreWrong xs

main :: IO ()
main = -- QuickCheck evensAreEven
  -- QuickCheck foosAreWrong
--  quickCheck noTracesAreValid
--  quickCheck prop_specialPair
  quickCheck $ verbose prop_listSh
--  quickCheck $ forAll randomTraceND noTracesAreValid

--------------------------------------------------------------------------------
-- Example of dependent shrinking, to compare against hedgehog
--------------------------------------------------------------------------------

data List =
  List
  { llength :: Int
  , list :: [Int]
  } deriving (Eq, Show)


-- shrinkSpecial :: (Int -> [Int]) -> [Int -> [Int]]
-- shrinkSpecial =  ->

-- shrinkSpecial :: List -> [List]
-- shrinkSpecial List {n, xs} =
--   [ List n' xs | n' <- shrink n
--                       , xs' <-
--   ]

instance Arbitrary List where
  arbitrary = do
    n <- choose (0, 10)
    xs <- vector n
    pure $! List { llength = n, list = xs }

  -- How to achieve the same shrinking behavior as for prop_specialPair?
  -- shrink List { n, xs } =
  --   [ List n xs' | xs' <- shrink xs ]

prop_specialPair :: List -> Bool
prop_specialPair List { list } = length list < 5
  -- case xs of
  --   x:_ -> x =/= 7
  --   _   -> property ()

data ListSh =
  ListSh
  { lengthSh :: Int
  , listSh :: [Int]
  , shrinks :: [ListSh]
  }
--  deriving Show

-- Use this if you don't want to show the smaller shrinks
instance Show ListSh where
  show ListSh { lengthSh, listSh } = show $ (lengthSh, listSh)


genListShOfLength :: Int -> Gen ListSh
genListShOfLength n = do
  xs <- vector n
  listShs <- traverse genListShOfLength (shrink n)
  pure ListSh { lengthSh = n
              , listSh = xs
              , shrinks = listShs
              }

lengthPreservingShrink :: Arbitrary a => [a] -> [[a]]
lengthPreservingShrink [] = []
lengthPreservingShrink (x:xs) =
  [ x':xs | x' <- shrink x ]
  ++
  [ x:xs' | xs' <- lengthPreservingShrink xs ]

shrinkListSh :: ListSh -> [ListSh]
shrinkListSh ListSh { lengthSh, listSh, shrinks } =
  -- concatMap shrinkListSh
  shrinks
  ++
  fmap (\xs -> ListSh lengthSh xs []) (lengthPreservingShrink listSh)


instance Arbitrary ListSh where
  arbitrary = do
    n <- choose (0, 15)
    genListShOfLength n

  shrink = shrinkListSh

prop_listSh :: ListSh -> Bool
prop_listSh ListSh { listSh } = length listSh < 5

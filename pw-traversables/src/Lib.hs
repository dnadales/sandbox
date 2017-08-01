-- | Playing with Traversables.
--
-- Sources:
--  - https://en.wikibooks.org/wiki/Haskell/Traversable

module Lib where

import           Control.Applicative
import           Control.Monad.State.Lazy
import           Data.Foldable
import           Data.Monoid

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- * Functors made for walking

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap f (Leaf a)       = Leaf (f a)
  fmap f (Branch lt rt) = Branch (fmap f lt) (fmap f rt)

instance Foldable Tree where
  foldr f z (Leaf a)       = f a z
  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z (Branch lt rt) = foldr f (foldr f z rt) lt

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a)       = Leaf <$> f a
  traverse f (Branch lt rt) = Branch <$> traverse f lt <*> traverse f rt


-- * Interpretations of Traversable

-- ** Building an intuition for sequences
--
-- > sequenceA [[0, 1, 2]]
-- > [[0],[1],[2]]
--
-- > sequenceA [[0, 1, 2], [3]]
-- > [[0,3],[1,3],[2,3]]
--
-- > sequenceA [[2, 3], [4, 5]]
-- > [[2,4],[2,5],[3,4],[3,5]]
--
-- > sequenceA [[0, 1], [2, 3], [4, 5]]
-- > [[0,2,4],[0,2,5],[0,3,4],[0,3,5],[1,2,4],[1,2,5],[1,3,4],[1,3,5]]

-- ** Exercises

-- | Matrix type. Matrices are represented in row-major order, e.g. the matrix:
--
-- ```
-- | 1 2 |
-- | 3 4 |
-- | 5 6 |
-- ```
--
-- Is represented as the following list:
--
-- > [[1, 2], [3, 4], [5, 6]]
--
newtype Matrix a = Matrix { mtx :: [[a]] } deriving Show

instance Functor Matrix where
  fmap f = Matrix . fmap (fmap f) . mtx

instance Foldable Matrix where
  -- foldr :: (a -> b -> b) -> b -> Matrix a -> b
  foldr f z = foldr f z . concat . mtx

instance Traversable Matrix where
  -- traverse :: Applicative f => (a -> f b) -> Matrix a -> f (Matrix b)
  traverse f m = Matrix <$> sequenceA (map (traverse f) (mtx m))

tstMatrix0 = Matrix [[0, 1], [2, 3]]

tstMatrix1 = Matrix [[5, 4, 3], [4, 0, 4], [7, 10, 3]]

transpose :: Matrix a -> Matrix a
transpose = Matrix . getZipList . traverse ZipList . mtx
-- Making sense out of this implementation:
--
-- > ZipList :: [a] -> ZipList a
-- > traverse ZipList :: Traversable t => t [b] -> ZipList (t b)
--
-- In particular when t = []:
--
-- > traverse ZipList :: Traversable t => [[b]] -> ZipList ([b])
--
-- > traverse ZipList [[0, 1], [2, 3]]
-- > traverse id [[0, 1], [2, 3]]

-- | The mapAccumL function behaves like a combination of fmap and foldl; it
-- applies a function to each element of a structure, passing an accumulating
-- parameter from left to right, and returning a final value of this
-- accumulator together with the new structure.
mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
-- (b -> a -> (a, c)) -> t b -> a -> (a, t c)
-- (b -> State a c) -> t b -> State a (t c)
-- traverse :: Applicative f => (b -> f c) -> t b -> f (t c)
mapAccumL f acc = undefined -- traverse (state . flip f)


moco :: Traversable t => (a -> (b -> (c, a))) -> t b -> State a (t c)
moco f = traverse (state . flip f)

mapAccumLS :: Traversable t =>  (b -> State a c) -> t b -> State a (t c)
mapAccumLS step t = traverse step t -- i.e. mapAccumLS = traverse

-- | This won't even compile :/ (taken from the Wiki...)
-- mapAccumL' :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
-- mapAccumL' step z t = runState (traverse (state . flip step) t) z

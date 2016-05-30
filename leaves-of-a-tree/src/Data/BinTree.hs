-- | A binary tree.

module Data.BinTree where

import           Control.Monad.State
import           Data.Functor.Fixedpoint

data BinTree a = Nil | Fork a (BinTree a) (BinTree a)
  deriving (Show, Eq)

-- | Computes the leaves of a binary tree by straightforward recursion,
-- using lists.
leaves :: BinTree a -> [a]
leaves Nil = []
leaves (Fork x Nil Nil) = [x]
leaves (Fork _ lt rt) = leaves lt ++ leaves rt

leavesT :: BinTree a -> [a]
leavesT = leavesT' []
  where
  leavesT' xs Nil = xs
  leavesT' xs (Fork x Nil Nil) = x : xs
  leavesT' xs (Fork _ lt rt) = leavesT' (leavesT' xs lt) rt

-- | Recursion using difference lists.
leavesC :: BinTree a -> [a] -> [a]
leavesC Nil = id
leavesC (Fork x Nil Nil) = \xs -> x : xs
leavesC (Fork _ lt rt) = leavesC lt . leavesC rt

-- | Computes the leaves of a binary tree by straightforward recursion,
-- using difference lists.
leaves' :: BinTree a -> [a]
leaves' t = leavesC t []

-- | Compute the leaves in a state monad.
leavesS :: BinTree a -> [a]
leavesS t = execState (leavesS' t) []
  where leavesS' :: BinTree a -> State [a] ()
        leavesS' Nil = return ()
        leavesS' (Fork x Nil Nil) = modify (\xs -> x:xs)
        leavesS' (Fork _ lt rt) = leavesS' lt >> leavesS' rt

-- | Generate a tree from a list.
mkTree :: [a] -> BinTree a
mkTree [] = Nil
mkTree (x:xs) = Fork x (mkTree lxs) (mkTree rxs)
  where (lxs, rxs) = splitAt ((length xs + 1) `div` 2) xs

-- | Functor whose fixed point is a binary tree.
data BinTreeF a b = NilF | ForkF a b b

instance Functor (BinTreeF a) where
  fmap f NilF = NilF
  fmap f (ForkF x lt rt) = ForkF x (f lt) (f rt)

-- | Same as BinTree, but defined as a fixed point of a functor.
type BinTreeE a = Fix (BinTreeF a)

toBinTreeE :: BinTree a -> BinTreeE a
toBinTreeE Nil = Fix NilF
toBinTreeE (Fork val lt rt) =
  Fix (ForkF val (toBinTreeE lt) (toBinTreeE rt))

mkTreeE :: [a] -> BinTreeE a
mkTreeE = toBinTreeE . mkTree

-- | Leaf enumeration routine implemented as a catamorphism.
leavesCata :: BinTreeE a -> [a]
leavesCata = cata gatherLeaves
  where gatherLeaves :: BinTreeF a [a] -> [a]
        gatherLeaves NilF = []
        gatherLeaves (ForkF x [] []) = [x]
        gatherLeaves (ForkF _ xs ys) = xs ++ ys

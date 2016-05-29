-- | A binary tree.

module Data.BinTree where

data BinTree a = Nil | Fork a (BinTree a) (BinTree a)
  deriving (Show, Eq)

-- | Computes the leaves of a binary tree.
leaves :: BinTree a -> [a]
leaves Nil = []
leaves (Fork x Nil Nil) = [x]
leaves (Fork _ lt rt) = (leaves lt) ++ (leaves rt)

leavesC :: BinTree a -> [a] -> [a]
leavesC Nil = id
leavesC (Fork x Nil Nil) = \xs -> x : xs
leavesC (Fork _ lt rt) = (leavesC lt) . (leavesC rt)

leaves' :: BinTree a -> [a]
leaves' t = leavesC t []

mkTree :: [a] -> BinTree a
mkTree [] = Nil
mkTree (x:xs) = Fork x (mkTree lxs) (mkTree rxs)
  where (lxs, rxs) = splitAt ((length xs + 1) `div` 2) xs

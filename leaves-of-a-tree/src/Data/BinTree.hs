-- | A binary tree.

module Data.BinTree where

data BinTree a = Nil | Fork a (BinTree a) (BinTree a)

-- | Computes the leaves of a binary tree.
leaves :: BinTree a -> [a]
leaves Nil = []
leaves (Fork x Nil Nil) = [x]
leaves (Fork _ lt rt) = (leaves lt) ++ (leaves rt)

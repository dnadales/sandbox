-- |

module LeavesOfATreeBench where

import           Criterion.Main
import           Data.BinTree

mkTree :: [a] -> BinTree a
mkTree [] = Nil
mkTree (x:xs) = Fork x (mkTree lxs) (mkTree rxs)
  where (lxs, rxs) = splitAt ((length xs + 1) `div` 2) xs

main :: IO ()
main = defaultMain [
  bgroup "leaves"
    [ bench "tree 0" $ whnf leaves (mkTree ([0 .. 20] :: [Integer]))
    , bench "tree 1" $ whnf leaves (mkTree ([0 .. 200] :: [Integer]))
    ]
  ]

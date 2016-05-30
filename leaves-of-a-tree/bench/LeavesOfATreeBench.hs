-- |

--module LeavesOfATreeBench where

import           Criterion.Main
import           Data.BinTree

tree0 :: BinTree Integer
tree0 = mkTree [0 .. 1000]

tree1 :: BinTree Integer
tree1 = mkTree [0 .. 1000000]

main :: IO ()
main = defaultMain [
  bgroup "leaves" [
      bench "leaves in lazy state monad tree 0" (nf leavesS tree0)
    , bench "leaves in lazy state monad tree 1" (nf leavesS tree1)
    , bench "leaves tree 0" $ whnf leaves tree0
    , bench "leaves tree 1" $ whnf leaves tree1
    , bench "leaves with continuation tree 0" $ whnf leaves' tree0
    , bench "leaves with continuation tree 1" $ whnf leaves' tree1
    ]
  ]

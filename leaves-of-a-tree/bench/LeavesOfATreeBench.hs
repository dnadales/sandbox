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
      bench "lazy state monad tree 0" (nf leavesS tree0)
    , bench "lazy state monad tree 1" (nf leavesS tree1)
    , bench "leaves tree 0" $ nf leaves tree0
    , bench "leaves tree 1" $ nf leaves tree1
    , bench "leaves with continuation tree 0" $ nf leaves' tree0
    , bench "leaves with continuation tree 1" $ nf leaves' tree1
    ]
  ]

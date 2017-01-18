-- | Benchmarks for Chapter 03.

--module Chapter03.Chapter03Bench where

import           Chapter03.Concat
import           Criterion.Main

xssLong :: [[Int]]
xssLong = mkLists 100

main :: IO ()
main = defaultMain [
  bgroup "concat" [
        bench "concatN nf" (nf concatN xssLong)
      , bench "concatL nf" (nf concatL xssLong)
      , bench "concat3 nf" (nf concat3 xssLong)
      ]
  ]

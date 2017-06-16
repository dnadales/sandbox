{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Criterion.Main
import           Criterion.Types
import qualified DirMetricsConcur as DMC
import qualified DirMetricsPipes  as DMP
import qualified DirMetricsTurtle as DMT

main :: IO ()
main =
  defaultMainWith benchConfig
  [
    bgroup "lines under a directory"
    [ bench "Turtle" $
      whnfIO (DMT.printLinesCountIn' fPath)
    , bench "Pipes" $
      whnfIO (DMP.printLinesCountIn fPath)
    , bench "Pipes.Concurrent 1 thread" $
      whnfIO (DMC.printLinesCountIn 1 fPath)
    , bench "Pipes.Concurrent 2 threads" $
      whnfIO (DMC.printLinesCountIn 2 fPath)
    , bench "Pipes.Concurrent 3 threads" $
      whnfIO (DMC.printLinesCountIn 3 fPath)
    , bench "Pipes.Concurrent 4 threads" $
      whnfIO (DMC.printLinesCountIn 4 fPath)
    , bench "Pipes.Concurrent 5 threads" $
      whnfIO (DMC.printLinesCountIn 5 fPath)
    , bench "Pipes.Concurrent 6 threads" $
      whnfIO (DMC.printLinesCountIn 6 fPath)
    , bench "Pipes.Concurrent 10 threads" $
      whnfIO (DMC.printLinesCountIn 10 fPath)
    , bench "Pipes.Concurrent 25 threads" $
      whnfIO (DMC.printLinesCountIn 25 fPath)
    ]
  ]
  where fPath = "/usr/include"
        benchConfig = defaultConfig
          { reportFile = Just "benchmark-output.html"
          , csvFile = Just "benchmark-output.csv"
          }

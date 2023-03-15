module Main (main) where

import           Examples.Control.Monad.IOSim.Basics (example, run)

main :: IO ()
main = do
  putStrLn "Example run with simulated IO: "
  putStrLn $ run example
  putStrLn "Example run with actual IO: "
  example

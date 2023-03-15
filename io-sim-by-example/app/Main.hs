module Main (main) where

import           Examples.Control.Monad.IOSim.Basics (run, sayHello)

main :: IO ()
main = do
  putStrLn "Example run with simulated IO: "
  putStrLn $ run sayHello
  putStrLn "Example run with actual IO: "
  sayHello

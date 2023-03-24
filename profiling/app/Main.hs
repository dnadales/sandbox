module Main (main) where

import           Lib

main :: IO ()
main = do
    print $ sum $ in_no_lambda xs ys
    print $ sum $ in_lambda    xs ys
    print $ sum $ out          xs ys
  where
    xs = [0..10000000]
    ys = [0..10000000]

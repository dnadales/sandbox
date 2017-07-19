module Main (main) where

import Data.Char
import Calc
import Data.Foldable

capitalize :: String -> String
capitalize = map toUpper

main :: IO ()
main = traverse_ print $ exprs `zip` map (parse capitalize) exprs
  where exprs = [ "15"
                , "x"
                , "let foo = 10 in foo + 1"
                , "x * (2 + 1)"
                , "let bar = 2 + 1 in bar + 1"
                ]

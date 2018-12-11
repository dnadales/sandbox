module Lib
    ( someFunc
    , module A
    , module B
    ) where

import           A (A (A))
import           B

someFunc :: IO ()
someFunc = putStrLn "someFunc"

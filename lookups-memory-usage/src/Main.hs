module Main where

import           Data.List (find)
import qualified Data.Map.Strict as Map

main :: IO ()
main =
  print $ result (10^6)
  where
    fromInteger = show
    result n = Map.lookup (fromInteger n) theMap
    -- result n = find (== fromInteger n) list
      where
        list = fromInteger <$> [0 .. (n :: Int)]
        theMap = Map.fromList
               $ zip list (repeat 1)

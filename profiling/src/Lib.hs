

module Lib
    ( in_no_lambda
    , in_lambda
    , out
    ) where

{- HLINT ignore "Use camelCase" -}

--------------------------------------------------------------------------------
-- Computing the sum inside, without lambdas
--------------------------------------------------------------------------------

in_no_lambda :: [Int] -> [Int] -> [Int]
in_no_lambda xs = fmap (aux_in_no_lambda xs)

aux_in_no_lambda :: [Int] -> Int -> Int
aux_in_no_lambda xs i = let sum_xs = {-# SCC "sum_in_no_lambda" #-} sum xs
                        in sum_xs + i

--------------------------------------------------------------------------------
-- Computing the sum inside, using lambdas
--------------------------------------------------------------------------------
in_lambda :: [Int] -> [Int] -> [Int]
in_lambda xs = fmap (aux_in_lambda xs)

aux_in_lambda :: [Int] -> Int -> Int
aux_in_lambda xs = let sum_xs = {-# SCC "sum_in_lambda" #-} sum xs
                   in \i -> sum_xs + i

--------------------------------------------------------------------------------
-- Computing the sum outside
--------------------------------------------------------------------------------
out :: [Int] -> [Int] -> [Int]
out xs = fmap (aux_out sum_xs)
  where sum_xs = {-# SCC "sum_out" #-} sum xs

aux_out :: Int -> Int -> Int
aux_out sum_xs i = sum_xs + i

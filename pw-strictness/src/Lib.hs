{-# LANGUAGE BangPatterns #-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  r <- time (pure undefined)
  putStrLn "someFunc"


time act = do
  r <- act
  pure $ r

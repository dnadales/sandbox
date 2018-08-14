{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ChimericLedgers.Value where

import           Data.Word (Word32)

newtype Value = Value Word32
    deriving (Eq, Ord, Num, Show)

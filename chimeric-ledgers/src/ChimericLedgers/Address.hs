{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ChimericLedgers.Address where

import           Data.Word (Word32)

newtype Address = Address Word32
    deriving (Eq, Ord, Num, Show)

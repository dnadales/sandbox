{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RankNTypes        #-}

-- | Examples taken from http://www.stephendiehl.com/posts/generics.html
module CookingClasses where

import           Data.Proxy
import           Data.Text    (Text)
import           GHC.Generics

data Pie = Pie
  { filling :: Filling
  , topping :: Maybe Topping
  } deriving (Show, Generic)

data Filling = Apple | Cherry | Pumpkin
  deriving (Show, Generic)

data Topping = IceCream | WhipCream
  deriving (Show, Generic)

-- * The problem
--
-- Enumerate all the types of pies we can make.

data Item
  = Item Text [Item]
  | Variant Text [Item]
  | Choice Text
  deriving (Show, Generic)

-- The code below does not work, and I'm don't know why, even after reading
-- https://stackoverflow.com/questions/39619805/rep-type-in-ghc-generics.
--
--
--
-- class Menu a where
--   menu :: a -> [Item]
--   default menu :: (Generic a, GMenu (Rep a)) => a -> [Item]
--   menu _ = gmenu (Proxy :: Proxy a)


-- gmenu :: forall a . (Generic a, GMenu (Rep a)) => Proxy a -> [Item]
-- gmenu _ = gopts (Proxy :: Proxy (Rep a))

-- class GMenu a where
--     gopts :: Proxy a -> [Item]

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

module Cereal where


import           Data.Bits
import           GHC.Generics


data Bit = O | I deriving Show

class Serialize a where
  put :: a -> [Bit]

  default put :: (Generic a, GSerialize (Rep a)) => a -> [Bit]
  put a = gput (from a)

  get :: [Bit] -> (a, [Bit])

  default get :: (Generic a, GSerialize (Rep a)) => [Bit] -> (a, [Bit])
  get xs = (to x, xs')
    where (x, xs') = gget xs

class GSerialize f where
  gput :: f a -> [Bit]
  gget :: [Bit] -> (f a, [Bit])

-- | Unit: used for constructors without arguments
instance GSerialize U1 where
  gput U1 = []
  gget xs = (U1, xs)

-- | Constants, additional parameters and recursion of kind *
instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
  gput (a :*: b) = gput a ++ gput b
  gget xs = (a :*: b, xs'')
    where (a, xs') = gget xs
          (b, xs'') = gget xs'

-- | Meta-information (constructor names, etc.)
instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
  gput (L1 x) = O : gput x
  gput (R1 x) = I : gput x
  gget (O:xs) = (L1 x, xs')
    where (x, xs') = gget xs
  gget (I:xs) = (R1 x, xs')
    where (x, xs') = gget xs

-- | Sums: encode choice between constructors
instance (GSerialize a) => GSerialize (M1 i c a) where
  gput (M1 x) = gput x
  gget xs = (M1 x, xs')
    where (x, xs') = gget xs

-- | Products: encode multiple arguments to constructors
instance (Serialize a) => GSerialize (K1 i a) where
  gput (K1 x) = put x
  gget xs = (K1 x, xs')
    where (x, xs') = get xs

instance Serialize Bool where
  put True  = [I]
  put False = [O]
  get (I:xs) = (True, xs)
  get (O:xs) = (False, xs)

--
-- Try it out. (Normally this would be in a separate module.)
--

data UserTree a = Node a (UserTree a) (UserTree a) | Leaf
  deriving (Generic, Show)

instance (Serialize a) => Serialize (UserTree a)

main = do
  let xs = put True
  print (fst . get $ xs :: Bool)
  let ys = put (Leaf :: UserTree Bool)
  print (fst . get $ ys :: UserTree Bool)
  let zs = put (Node False Leaf Leaf :: UserTree Bool)
  print (fst . get $ zs :: UserTree Bool)

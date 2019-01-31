{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE FlexibleInstances        #-}

module Example where

import Data.Data
import Data.Typeable
import Control.Applicative

import           GHC.Generics

data Foo = Foo deriving (Data, Typeable, Show, Generic)

data Bar = Bar deriving (Data, Typeable, Show, Generic)

data Baz = Baz [Foo] (Bar, Bar) deriving (Data, Typeable, Show, Generic)

data Input = Input Int deriving (Data, Typeable, Show, Generic)

data Output = Output Double deriving (Data, Typeable, Show, Generic)

data Tx = Tx [Input] [Output] deriving (Data, Typeable, Show, Generic)

newtype Size a = Size { unSize :: Int }

plus :: Size a -> Size b -> Size c
plus (Size s0) (Size s1) = Size (s0 + s1)

sumFields :: Data a => a -> Size a
sumFields baz = gfoldl step fcstr baz
  where
    step :: forall d b. Data d =>  Size (d -> b) -> d -> Size b
    step tot d = tot `plus` sumFields d

    fcstr :: forall g. g -> Size g
    fcstr _  = Size 0

newtype TypeReps a = TypeReps { getTypes :: [TypeRep] } deriving (Show)

(<++>) :: TypeReps a -> TypeReps b -> TypeReps c
(TypeReps xs) <++> (TypeReps ys) = TypeReps (xs ++ ys) -- TODO: we could use a more efficient structure.

typeReps :: (Data a, Typeable a) => a -> TypeReps a
typeReps a = gfoldl step fcstr a
  where
    step :: forall d b. Data d =>  TypeReps (d -> b) -> d -> TypeReps b
    step tot d = tot <++> typeReps d

    fcstr :: forall g . g -> TypeReps g
    fcstr g  = TypeReps [typeOf a]

-- This won't work either!
typeReps2 :: forall a. (Typeable a, Data a) => a -> [TypeRep]
typeReps2 = getConst . gfoldl app constr where

  -- app :: [TypeRep] -> d -> [TypeRep]                            -- informal type
  -- app :: Data d => Const [TypeRep] _ -> d -> Const [TypeRep] _  -- actual type
  --
  -- Collect the types from the field d and append them to the result.
  app (Const tys) d = Const (tys ++ typeReps2 d)

  -- constr :: _ -> Const [TypeRep] _
  --
  -- Starting state: we have to at least collect the current type.
  -- The argument of constr is the constructor of the value we are traversing,
  -- it is useless here because we don't want to construct anything.
  constr _ = Const [typeOf (undefined :: a)]

--------------------------------------------------------------------------------
-- Attempt using generics
--------------------------------------------------------------------------------

class Typeable a => HasTypeReps a where
  typeReps3 :: a -> [TypeRep]

  default typeReps3 :: (Generic a, GHasTypeReps (Rep a)) => a -> [TypeRep]
  typeReps3 a = typeOf a: gTypeReps (from a)

class GHasTypeReps f where
  gTypeReps :: f a -> [TypeRep]

instance GHasTypeReps U1 where
  gTypeReps U1 = []

instance (GHasTypeReps a, GHasTypeReps b) => GHasTypeReps (a :*: b) where
  gTypeReps (a :*: b) = gTypeReps a ++ gTypeReps b

instance (GHasTypeReps a, GHasTypeReps b) => GHasTypeReps (a :+: b) where
  gTypeReps (L1 a) = gTypeReps a
  gTypeReps (R1 b) = gTypeReps b

-- | We do need to do anything for the metadata.
instance (GHasTypeReps a) => GHasTypeReps (M1 i c a) where
    gTypeReps (M1 x) = gTypeReps x

-- | And the only interesting case, get the type of a type constructor
instance (HasTypeReps a) => GHasTypeReps (K1 i a) where
    gTypeReps (K1 x) = typeReps3 x

instance HasTypeReps a => HasTypeReps [a] where
  typeReps3 xs = typeOf xs: concatMap typeReps3 xs

instance (HasTypeReps a, HasTypeReps b) => HasTypeReps (a, b) where
  typeReps3 t@(a, b) = [typeOf t, typeOf a, typeOf b]

instance HasTypeReps Char where
  typeReps3 x = [typeOf x]
instance HasTypeReps Int where
  typeReps3 x = [typeOf x]

instance HasTypeReps Double where
  typeReps3 x = [typeOf x]

instance HasTypeReps Input
instance HasTypeReps Output
instance HasTypeReps Tx

--------------------------------------------------------------------------------
-- https://stackoverflow.com/questions/54444559/get-all-the-typereps-in-a-value-using-generic-programming/54446912#54446912
--------------------------------------------------------------------------------

-- | Returns 'Just' only for lists
--
-- This can surely be done more efficiently, but it does the job.
listTypeReps :: Data a => a -> Maybe [TypeRep]
listTypeReps x

  | typeRepTyCon (typeOf x) == listTyCon
  , toConstr x == toConstr ([] :: [()])   -- empty list
  = Just []

  | typeRepTyCon (typeOf x) == listTyCon
  , toConstr x == toConstr [()]           -- cons
  , [headTs, _] <- gmapQ typeReps4 x
  , [_, Just tailTs] <- gmapQ listTypeReps x
  = Just (headTs ++ tailTs)

  | otherwise
  = Nothing

listTyCon :: TyCon
listTyCon = typeRepTyCon (typeOf ([] :: [()]))

-- | Get the types of subterms
typeReps4 :: Data a => a -> [TypeRep]
typeReps4 x = typeOf x : case listTypeReps x of
                          Just ts -> ts
                          Nothing -> concat (gmapQ typeReps4 x)

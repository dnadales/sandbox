{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter11.OpenProducts where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Data.Vector ((//))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import Fcf

data Any (f :: k -> Type) where
  Any :: f t -> Any f

unAny :: forall f t . Any f -> f t
unAny (Any ft) = unsafeCoerce ft

data OpenProduct (f  :: t -> Type)
                 (ts :: [(Symbol, t)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

insert
  :: Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct vs) = OpenProduct $ V.cons (Any ft) vs

openProduct0 :: OpenProduct Maybe '[ '("key", String) ]
openProduct0 = insert (Key @"key") (Just "hello") nil

openProduct1 :: OpenProduct Maybe '[ '("another", Bool), '("key", String) ]
openProduct1 = insert (Key @"another") (Just True) openProduct0

-- Note that this preliminary version of 'insert' allows us to insert
-- duplicated keys.
openProduct2 :: OpenProduct Maybe '[ '("key", Bool), '("key", String) ]
openProduct2 = insert (Key @"key") (Just True) openProduct0

instance Show (Any f) => Show (OpenProduct f ts) where
  show (OpenProduct vs) = concat $ fmap show vs

-- QUESTION: how can we define an any instance?
--
-- instance (forall t . Show (f t)) => Show (Any f) where
--   show (Any ft) = show ft

-- | Type family that computes whether a key would be unique.
type UniqueKey (key :: Symbol) (ts :: [(Symbol, t)])
  = Null =<< Filter (TyEq key <=< Fst) ts

-- Try this out!
--
-- >>> :set -XDataKinds
-- >>> import Fcf
-- >>> :kind! Fcf.Eval (UniqueKey ("hello") '[  ])
-- Fcf.Eval (UniqueKey ("hello") '[  ]) :: Bool
-- = 'True
--
-- >>> :kind! Fcf.Eval (UniqueKey ("hello") '[ '("hello", Bool) ])
-- Fcf.Eval (UniqueKey ("hello") '[ '("hello", Bool) ]) :: Bool
-- = 'False
--
-- >>> :kind! Fcf.Eval (UniqueKey ("hello") '[ '("world", Char) ])
-- Fcf.Eval (UniqueKey ("hello") '[ '("world", Char) ]) :: Bool
-- = 'True
--

insert'
  :: Eval (UniqueKey key ts) ~ 'True
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('(key, t) ': ts)
insert' = insert

-- Try this out!
--
-- >>> :set -XTypeApplications
-- >>> :t insert' (Key @"key") (Just True) openProduct0
-- <interactive>:1:1: error:
-- • Couldn't match type ‘'False’ with ‘'True’
--     arising from a use of ‘insert'’
-- • In the expression: insert' (Key @"key") (Just True) openProduct0

type FindElem (key :: Symbol) (ts :: [(Symbol, t)])
  = Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem :: forall key ts. KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

type LookupType (key :: k) (ts :: [(k, t)])
  = FromMaybe Stuck =<< Lookup key ts

get
  :: forall key ts f
   . KnownNat (FindElem key ts)
  => Key key
  -> OpenProduct f ts
  -> f (Eval (LookupType key ts))
get _ (OpenProduct v) =
  unAny $ V.unsafeIndex v $ findElem @key @ts

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)])
  = SetIndex (FindElem key ts) '(key, t) ts

update
  :: forall key f t ts
   . KnownNat (FindElem key ts) -- TODO: what would happen if we leave out this constraint?
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct vs) = OpenProduct $ vs // [(findElem @key @ts, Any ft)]

-- Try this out
--
-- >>> :set -XFlexibleContexts
-- >>> :set -XAllowAmbiguousTypes
-- >>> :t update (Key @"key") (Just "bye") openProduct0
-- update (Key @"key") (Just "bye") openProduct0
--  :: OpenProduct Maybe '[ '("key", [Char])]
--
-- >>> :t update (Key @"key") (Just (23 :: Int)) openProduct0
-- update (Key @"key") (Just (23 :: Int)) openProduct0
--  :: OpenProduct Maybe '[ '("key", Int)]
--
-- >>> :t update (Key @"boo") (Just (23 :: Int)) openProduct0
-- <interactive>:1:1: error:
--     • No instance for (KnownNat Stuck) arising from a use of ‘update’
--     • In the expression:
--         update (Key @"boo") (Just (23 :: Int)) openProduct0
--
-- >>> get (Key @"key") openProduct0
-- Just "hello"
--
-- >>> get (Key @"key") $ update (Key @"key") (Just (23 :: Int)) openProduct0
-- Just 23
--

-- | Exercise 11.3-i: implement 'delete' for 'OpenProduct's.
--
delete
  :: forall key f ts
   . KnownNat (FindElem key ts) -- Note that if you leave this out the code won't compile.
  => Key key
  -> OpenProduct f ts
  -> OpenProduct f (Eval (DeleteElement key ts))
delete _ (OpenProduct vs) =
  OpenProduct $ V.take i vs V.++ V.drop (i+1) vs
  where
    i = findElem @key @ts

type DeleteElement (key :: Symbol) (ts :: [(Symbol, k)])
  = Filter (Not <=< TyEq key <=< Fst) ts

-- Try this out!
--
-- >>> :t delete (Key @"key") $ openProduct0
-- delete (Key @"key") $ openProduct0 :: OpenProduct Maybe '[]
--
-- >>> :t delete (Key @"notHere") $ openProduct0
-- <interactive>:1:1: error:
--     • No instance for (KnownNat Stuck) arising from a use of ‘delete’
--     • In the expression: delete (Key @"notHere")
--       In the expression: delete (Key @"notHere") $ openProduct0
--

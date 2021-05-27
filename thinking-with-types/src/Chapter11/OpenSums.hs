{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter11.OpenSums where

import Data.Kind (Type)
import Data.Proxy

import GHC.TypeLits hiding (type (+))
import Unsafe.Coerce

import Data.Functor.Identity

import Fcf

data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum
    :: Int
    -> f t
    -> OpenSum f ts

-- It is useful to be able to show the open sums, so we define an instance
-- for the terms of this type.

instance (Show (f t), Show (OpenSum f ts)) => Show (OpenSum f (t ': ts)) where
  show x =
    case decompose x of
      Left ft  -> show ft
      Right x' -> show x'


instance (Show (OpenSum f '[])) where
  show _ = "[]"

-- | Find element tries to find a kind @k@ in @ts@.
type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts

-- | Determine if a type is an element in the given type list.
type Member t ts = KnownNat (Eval (FindElem t ts))


-- | Try this out!
--
-- >>> :set -XDataKinds
-- >>> :kind! Member Bool '[ Int, Bool ]
-- Member Bool '[ Int, Bool ] :: Constraint
-- = GHC.TypeNats.KnownNat 1
--
-- >>> :kind! Member Double  '[ Int, Bool ]
-- Member Double  '[ Int, Bool ] :: Constraint
-- = GHC.TypeNats.KnownNat Fcf.Utils.Stuck

-- | Reification of kind Nat at the term level.
findElem :: forall t ts . Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

-- | Construct an open sum
--
-- >>> :set -XFlexibleContexts
-- >>> import Data.Functor.Identity
-- >>> x=inj (Identity "hello") :: OpenSum Identity '[String]
--
inj :: forall f t ts . Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

-- | Take something out from an open sum
--
-- To take something out from the open sum you need to specify the existential
-- type, namely @t@.
--
-- >>> prj x :: Maybe (Identity String)
-- Just (Identity "hello")
--
-- >>> prj x :: Maybe (Identity Int)
--
--
-- If you don't specify the existential type, GHC will not know what @t@ should be.
--
-- >>> prj x
-- <interactive>:31:1: error:
--     • No instance for (GHC.TypeNats.KnownNat Fcf.Utils.Stuck)
--         arising from a use of ‘it’
--     • In the first argument of ‘print’, namely ‘it’
--       In a stmt of an interactive GHCi command: print it
--
prj :: forall f t ts . Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i ft) =
  if i == findElem @t @ts
  then Just $ unsafeCoerce ft
  else Nothing

-- | Reduce an open sum regardless of what's inside it.
--
-- >>> decompose x
decompose
  :: OpenSum f (t ': ts)
  -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 ft) = Left  $ unsafeCoerce ft
decompose (UnsafeOpenSum n ft) = Right $ UnsafeOpenSum (n - 1) ft

-- | Exercise 11.2-i: write 'weaken'
weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum n ft) =
  -- The type of the open sum is in position @n@ in @ts@, which means that is
  -- in position @n + 1@ in @t ': ts@.
  UnsafeOpenSum (n + 1) ft

hello :: OpenSum Identity '[String]
hello = inj (Identity "hello")

answer :: OpenSum Identity '[Int]
answer = inj (Identity (42 :: Int))

openlist :: [OpenSum Identity '[Int, String]]
openlist = [ weaken hello
           , inj (Identity (42 :: Int))
           ] -- , weaken answer] This won't work because the type list will not
             -- be the same: weaken answer will give you the '[x, Int] type,
             -- instead of '[Int, String].

-- | Perform the same logic regardless of what's inside an @OpenSum@.
match
  :: forall f ts b
   . (forall t . f t -> b)
  -> OpenSum f ts
  -> b
match g (UnsafeOpenSum _ ft) = g ft

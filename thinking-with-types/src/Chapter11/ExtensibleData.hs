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

module Chapter11.ExtensibleData where

import Data.Kind (Type)
import Data.Proxy

import GHC.TypeLits hiding (type (+))
import Unsafe.Coerce

import Fcf

data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum
    :: Int
    -> f t
    -> OpenSum f ts

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

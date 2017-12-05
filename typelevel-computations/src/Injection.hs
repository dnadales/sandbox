{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Code taken from:
--
--     http://mpickering.github.io/posts/2014-12-20-closed-type-family-data-types.html
--
-- Calculating where an element (functor) should be injected at the type level.

module Injection where

import           Data.Proxy

-- | Co-product.
data (f :+: g) e = Inl (f e) | Inr (g e)

data Crumbs = Here | L Crumbs | R Crumbs

data Res = Found Crumbs | NotFound

-- Is @e@ an element of @f@?
type family Elem e f :: Res where
    Elem e e = Found Here
    Elem e (l :+: r) = Choose (Elem e l) (Elem e r)

type family Choose e f :: Res where
    Choose (Found a) b = Found (L a)
    Choose a (Found b) = Found (R b)
    Choose a b = NotFound

class MakeInj (res :: Res) f g where
    mkInj :: Proxy res -> f a -> g a

instance MakeInj (Found Here) f f where
    mkInj _ = id

instance MakeInj (Found p) f l => MakeInj (Found (L p)) f (l :+: r) where
    mkInj _ = Inl . mkInj (Proxy :: Proxy (Found p))

instance MakeInj (Found p) f r => MakeInj (Found (R p)) f (l :+: r) where
    mkInj _ = Inr . mkInj (Proxy :: Proxy (Found p))

type f :<: g = MakeInj (Elem f g) f g

-- This won't compile:
--
-- > instance MakeInj (MakeInj (Found p) f l) => MakeInj (Found (L p)) f (l :+: r) where
-- >    mkInj _ = Inl . mkInj (Proxy :: Proxy (Found p))
--
-- >     • Expected kind ‘Res’,
-- >        but ‘MakeInj ('Found p) f gl’ has kind ‘Constraint’
-- >    • In the first argument of ‘MakeInj’, namely
-- >        ‘MakeInj (Found p) f gl’
-- >      In the instance declaration for
-- >        ‘MakeInj (Found (L p)) f (gl :+: gr)’



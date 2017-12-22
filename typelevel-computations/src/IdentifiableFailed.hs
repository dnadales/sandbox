-- | See the question here:
--
--     https://stackoverflow.com/questions/47453657/extracting-an-id-with-ghc-generics
--
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Identifiable where

import           Data.Proxy
import           GHC.Generics

type Name = String

newtype Id = Id { _id :: Int }

data VarId = VarId Name Id SortId
data FuncId = FuncId Name Id [SortId] SortId
newtype SortId = Name Id
-- ... and 20 more of these things

data Identifier = IdVar VarId
                | IdFunc FuncId
                | IdSort SortId
                -- ... and 20 more of these things

class Identifiable e where
    getId :: e -> Id

    -- And here is where everything fails: @Rep e@ has kind @* -> *@, whereas
    -- @HasId@ expects a type (with kind :: I'd guess).
    -- default getId :: (Generic e, MakeIdentifiable (HasId (Rep e)) (Rep e)) => e -> Id
    -- getId = (mkGetId (Proxy :: Proxy (HasId (Rep r f)))) . from

-- This won't work either:
--
getId' :: forall e f . (Generic e, MakeIdentifiable (HasId (Rep e f)) (Rep e)) => e -> Id
getId' _ x = mkGetId (Proxy :: Proxy (HasId (Rep e f))) (from x)
--
-- >    • Could not deduce (MakeIdentifiable (HasId (Rep e f0)) (Rep e))
-- >      from the context: (Generic e,
-- >                         MakeIdentifiable (HasId (Rep e f)) (Rep e))
--

data Crumbs = Here | GoLeft Crumbs | GoRight Crumbs

data Res = Found Crumbs | NotFound

type family HasId e :: Res where
    HasId Id = Found Here
    HasId (M1 i c a p) = HasId (a p)
    HasId (U1 p) = NotFound
    HasId (K1 i t p) = HasId t
    HasId ((l :+: r) p) = Choose (HasId (l p)) (HasId (r p))
    HasId ((l :*: r) p) = Choose (HasId (l p)) (HasId (r p))
    HasId a = NotFound

type family Choose (a :: Res)  (b :: Res) :: Res where
    Choose (Found a) b = Found (GoLeft a)
    Choose a (Found b) = Found (GoRight b)
    Choose NotFound NotFound = NotFound

class MakeIdentifiable (res :: Res) f where
    mkGetId :: Proxy res -> f e -> Id

instance MakeIdentifiable (Found Here) (K1 i Id) where
    mkGetId _ (K1 x) = x

instance MakeIdentifiable (Found c) a => MakeIdentifiable (Found (GoLeft c)) (a :+: b) where
    -- ScopedTypeVariables needed for having the @c@ in the implementation
    -- refer to the same @c@ in the instance constraints.
    mkGetId _ (L1 a) = mkGetId (Proxy :: Proxy (Found c)) a

instance MakeIdentifiable (Found c) b => MakeIdentifiable (Found (GoRight c)) (a :+: b) where
    mkGetId _ (R1 b) = mkGetId (Proxy :: Proxy (Found c)) b

instance MakeIdentifiable (Found c) a => MakeIdentifiable (Found (GoLeft c)) (a :*: b) where
    mkGetId _ (a :*: b) = mkGetId (Proxy :: Proxy (Found c)) a

instance MakeIdentifiable (Found c) b => MakeIdentifiable (Found (GoRight c)) (a :*: b) where
    mkGetId _ (a :*: b) = mkGetId (Proxy :: Proxy (Found c)) b

instance MakeIdentifiable (Found c) a => MakeIdentifiable (Found c) (M1 i d a) where
    mkGetId _ (M1 x) = mkGetId (Proxy :: Proxy (Found c)) x

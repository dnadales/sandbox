-- | See the question here:
--
--     https://stackoverflow.com/questions/47453657/extracting-an-id-with-ghc-generics
--
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
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

newtype Id = Id { _id :: Int } deriving (Generic, Eq, Show)

data VarId = VarId Name Id SortId deriving (Generic, Eq, Show)
data FuncId = FuncId Name Id [SortId] SortId deriving (Generic, Eq, Show)
data SortId = SortId Name Id deriving (Generic, Eq, Show)
-- ... and 20 more of these things

data Identifier = IdVar VarId
                | IdFunc FuncId
                | IdSort SortId
                deriving (Generic, Eq, Show)
                -- ... and 20 more of these things

-- Apparently this won't work using @default@ (somehow the type @e@ in the
-- signature is not equated to the type @e@ in the implementation).
getId' :: forall e . (Generic e, Identifiable (HasId (Rep e)) (Rep e)) => e -> Id
getId' x  = mkGetId (Proxy :: Proxy (HasId (Rep e))) (from x)

data Crumbs = Here | GoLeft Crumbs | GoRight Crumbs
data Res = Found Crumbs | NotFound

type family Choose (a :: Res)  (b :: Res) :: Res where
    Choose (Found a) b = Found (GoLeft a)
    Choose a (Found b) = Found (GoRight b)
    Choose NotFound NotFound = NotFound

-- The kind * -> * is needed to be able to work with the representations used
-- at Generics.
type family HasId (e :: * -> *) :: Res where
    HasId (M1 i c t) = HasId t
    HasId (K1 i Id) = Found Here
    HasId (l :+: r) = Choose (HasId l) (HasId r)
    HasId (l :*: r) = Choose (HasId l) (HasId r)
    HasId a = NotFound

class Identifiable (res :: Res) f where
    mkGetId :: Proxy res -> f e -> Id

instance Identifiable (Found Here) (K1 i Id) where
    mkGetId _ (K1 x) = x

instance Identifiable (Found c) a => Identifiable (Found (GoLeft c)) (a :+: b) where
    -- ScopedTypeVariables needed for having the @c@ in the implementation
    -- refer to the same @c@ in the instance constraints.
    mkGetId _ (L1 a) = mkGetId (Proxy :: Proxy (Found c)) a

instance Identifiable (Found c) b => Identifiable (Found (GoRight c)) (a :+: b) where
    mkGetId _ (R1 b) = mkGetId (Proxy :: Proxy (Found c)) b

instance Identifiable (Found c) a => Identifiable (Found (GoLeft c)) (a :*: b) where
    mkGetId _ (a :*: b) = mkGetId (Proxy :: Proxy (Found c)) a

instance Identifiable (Found c) b => Identifiable (Found (GoRight c)) (a :*: b) where
    mkGetId _ (a :*: b) = mkGetId (Proxy :: Proxy (Found c)) b

instance Identifiable (Found c) a => Identifiable (Found c) (M1 i d a) where
    mkGetId _ (M1 x) = mkGetId (Proxy :: Proxy (Found c)) x

-- | Problem:
--
-- This works:
--
-- λ> getId' (SortId "hello" (Id 10))
-- > Id {_id = 10}
--
-- Bot this won't:
--
-- λ> getId' (IdSort (SortId "hello" (Id 10)))
--
--
-- > No instance for (Identifiable 'NotFound ...)

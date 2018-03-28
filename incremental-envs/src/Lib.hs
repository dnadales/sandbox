{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import           Data.Proxy

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Name = String

data Fruit = Orange | Pear | Apple

data Vegetable = Cucumber | Carrot | Spinach

data Legume = Lentils | Chickpeas | BlackEyedPeas

class HasFruit e where
    getFruit :: e -> Name -> Maybe Fruit

class HasVegetable e where
    getVegetable :: e -> Name -> Maybe Vegetable

class HasLegume e where
    getLegume :: e -> Name -> Maybe Legume

data Smootie

mkSmoothie :: (HasFruit e, HasVegetable e) => e -> Smootie
mkSmoothie = undefined

data Salad

mkSalad :: (HasVegetable e, HasLegume e) => e -> Salad
mkSalad = undefined

-- instance HasFruit [Fruit] where
--     getFruit = undefined

instance HasVegetable [Vegetable] where
    getVegetable = undefined

instance HasLegume [Legume] where
    getLegume = undefined

-- instance HasFruit e0 => HasFruit (e0, e1, e2) where
--     getFruit (e0, _, _) = getFruit e0

-- instance HasVegetable e1 => HasVegetable (e0, e1, e2) where
--     getVegetable (_, e1, _) = getVegetable e1

-- instance HasLegume e2 => HasLegume (e0, e1, e2) where
--     getLegume (_, _, e2) = getLegume e2

-- cook :: (Smootie, Salad)
-- cook = let ingredients = ([Orange], [Cucumber], [BlackEyedPeas]) in
--     (mkSmoothie ingredients, mkSalad ingredients)

-- This, even though cumbersome, works. But now assume that we decide to
-- add a 'mkStew', which requires some 'HasMeat' instance. Then we'll have to
-- change all the instances above. Furthermore, if we would like to use
-- 'mkSmothie' in isolation, we cannot just pass ([Orange], [Cucumber]) since
-- there is no instance defined for it.

-- I could define:
--
data Sum a b = Sum a b
--
-- and instances like
--
-- > instance HasFruit e0 => HasFruit (Sum e0 e1) where
-- >    getFruit (Sum e0 _) = getFruit e0

-- > instance HasVegetable e1 => HasVegetable (Sum e0 e1) where
-- >    getVegetable (Sum _ e1) = getVegetable e1

-- > instance HasLegume e1 => HasLegume (Sum e0 e1) where
-- >    getLegume (Sum _ e1) = getLegume e1

-- But this won't work (No instance for `HasVegetable [Legume]`)
-- cook1 :: (Smootie, Salad)
-- cook1 = let ingredients = Sum [Orange] (Sum [Cucumber] [BlackEyedPeas]) in
--     (mkSmoothie ingredients, mkSalad ingredients)

-- And This instance will overlap!
-- instance HasVegetable e0 => HasVegetable (Sum e0 e1) where
--     getVegetable (Sum e0 e1) = getVegetable e0

data Crumbs = Here | GoLeft Crumbs | GoRight Crumbs
data Res = Found Crumbs | NotFound

class HasFruit' (res :: Res) e where
    getFruit' :: Proxy res -> e -> Name -> Maybe Fruit

instance HasFruit' ('Found 'Here) [Fruit] where
    getFruit' _ = undefined

instance HasFruit' ('Found c) e0 =>
    HasFruit' ('Found ('GoLeft c)) (Sum e0 e1) where
    getFruit' = undefined

instance HasFruit' ('Found c) e1 =>
    HasFruit' ('Found ('GoRight c)) (Sum e0 e1) where
    getFruit' = undefined

instance (F a ~ res, HasFruit' res a) => HasFruit a where
    getFruit = getFruit' (Proxy :: Proxy res)

type family (F a) :: Res where
    F [Fruit]   = 'Found 'Here
    F (Sum a b) = Search (F a) (F b)
    F c         = 'NotFound

type family Search (a :: Res) (b :: Res) :: Res where
    Search ('Found a) b        = 'Found ('GoLeft a)
    Search a ('Found b)        = 'Found ('GoRight b)
    Search 'NotFound 'NotFound = 'NotFound

f :: (HasFruit e) => e -> Smootie
f = undefined

g :: Smootie
g = f (Sum [Orange] [Carrot])

h :: Smootie
h = f (Sum [Carrot] [Orange])

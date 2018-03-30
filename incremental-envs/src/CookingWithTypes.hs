{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module CookingWithTypes where

import           Data.Type.Bool
import           GHC.TypeLits   (ErrorMessage ((:<>:), ShowType, Text),
                                 TypeError)

type Name = String
data Fruit = Orange | Pear | Apple deriving (Show, Eq)
data Vegetable = Cucumber | Carrot | Spinach deriving (Show, Eq)
data Legume = Lentils | Chickpeas | BlackEyedPeas deriving (Show, Eq)

data Tree a = Leaf a | Node (Tree a) (Tree a)

-- | Type family that represents the contents of a basket.
type family Contents basket :: Tree *

type instance Contents [Fruit] = 'Leaf Fruit
type instance Contents [Vegetable] = 'Leaf Vegetable
type instance Contents [Legume] = 'Leaf Legume

-- | Combinator for baskets.
data a :& b = a :& b

-- | Define the contents of a composite basket.
--
--
-- You can try this out:
--
-- > λ> :kind! Contents ([Fruit] :& [Vegetable])
-- > Contents ([Fruit] :& [Vegetable]) :: Tree *
-- > = 'Node ('Leaf Fruit) ('Leaf Vegetable)
type instance Contents (a :& b) = 'Node (Contents a) (Contents b)

-- | Compute when two elements are the same.
type family y == x :: Bool where
    x == x = 'True
    x == y = 'False

-- | Compute when something (a type?) is in a 'Tree'.
--
-- > λ> :kind! In Fruit (Contents [Fruit])
-- > In Fruit (Contents [Fruit]) :: Bool
-- > = 'True
--
type family In (x :: *) (ys :: Tree *) :: Bool where
    In x ('Leaf y)   = x == y
    In x ('Node l r) = In x l || In x r

-- | Is 'item' in the 'basket'.
class (In item (Contents basket) ~ 'True) => Has item basket where
    get :: basket -> Name -> Maybe item

instance Has Fruit [Fruit] where
    get = undefined

instance Has Vegetable [Vegetable] where
    get = undefined

instance Has Legume [Legume] where
    get = undefined

-- | Computation on a composite basket.
class ( In item (Contents a) ~ inA -- 'inA': is the item in 'Contents a'?
      , In item (Contents b) ~ inB -- 'inB': is the item in 'Contents b'?
      , (inA || inB) ~ 'True       -- The item has to be in one of the baskets.
      ) =>
      PairHas item a b inA inB where
    getPair :: (a :& b) -> Name -> Maybe item

instance (Has item a, In item (Contents b) ~ 'False)
         => PairHas item a b 'True 'False where
    getPair (a :& _) = get a

instance (In item (Contents a) ~ 'False, Has item b)
         => PairHas item a b 'False 'True where
    getPair (_ :& b) = get b

instance PairHas item a b inA inB => Has item (a :& b) where
    get = undefined

data Smootie

mkSmootie :: (Has Fruit e, Has Vegetable e) => e -> Smootie
mkSmootie = undefined

data Salad

mkSalad :: (Has Vegetable e, Has Legume e) => e -> Salad
mkSalad = undefined

cook :: (Smootie, Salad)
cook = let ingredients = [Orange] :& [Cucumber] :& [BlackEyedPeas] in
    (mkSmootie ingredients, mkSalad ingredients)

-- | This instance allows to give a more informative type error when an item is
-- not found in the bag.
--
-- > cook2 :: (Smootie, Salad)
-- > cook2 = let ingredients = [Cucumber] :& [BlackEyedPeas] :& [Carrot] in
-- >    (mkSmootie ingredients, mkSalad ingredients)
-- > 'False ~ 'True is needed since otherwise you'll get the error:
--
-- > Couldn't match type ‘'False’ with ‘'True’
-- >   arising from the superclasses of an instance declaration
-- > • In the instance declaration for ‘PairHas item a b 'False 'False’
--
-- The problem is that the super-class is demanding:
--
-- > (inA || inB) ~ 'True
--
-- And since we (the GHC compiler either) cannot deduce:
--
-- > ('False || 'False) ~ 'True
--
-- We have to fake that here.
instance ( TypeError (     'Text "Could not find "
                      :<>: 'ShowType item
                      :<>: 'Text " in "
                      :<>: 'ShowType (a :& b) )
         , In item (Contents a) ~ 'False
         , In item (Contents b) ~ 'False
         , 'False ~ 'True )
         => PairHas item a b 'False 'False where
    getPair = undefined

-- | This instance introduces an error when an item is found at more than one place:
--
--
-- > cook3 :: (Smootie, Salad)
-- > cook3 = let ingredients = [Orange] :& [BlackEyedPeas] :& [Carrot] :& [Apple] in
-- >   (mkSmootie ingredients, mkSalad ingredients)
instance ( TypeError (     'Text "Found more than one "
                      :<>: 'ShowType item
                      :<>: 'Text " in "
                      :<>: 'ShowType (a :& b) )
         , In item (Contents a) ~ 'True
         , In item (Contents b) ~ 'True )
         => PairHas item a b 'True 'True where
    getPair = undefined


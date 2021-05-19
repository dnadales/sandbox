{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter10.Defunctionalization where

import Prelude hiding (fst)

fst :: (a, b) -> a
fst (a, _) = a

-- | Defuntionalized equivalent of 'fst'. In this context, "defuntionalization"
-- is the process by which an __instantiation of__ a polymorphic function is
-- replaced with a specialized label.
data Fst a b = Fst (a, b)

-- | How do we evaluate these labels? By means of the 'eval' function.
class Eval l t | l -> t where
  eval :: l -> t

instance Eval (Fst a b) a where
  eval (Fst (a, _)) = a

-- | Exercise 10.1-i: defunctionalize 'listToMaybe'
listToMaybe :: [a] -> Maybe a
listToMaybe (a:_) = Just a
listToMaybe _     = Nothing

data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe (a:_)) = Just a
  eval (ListToMaybe _    ) = Nothing

--------------------------------------------------------------------------------
-- Defunctionalization of higher order functions
--------------------------------------------------------------------------------
data MapList dfb a = MapList (a -> dfb) [a]

instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  eval (MapList _f []    ) = []
  eval (MapList  f (x:xs)) = eval (f x) : eval (MapList f xs)

-- Try it out!
--
-- > eval (MapList Fst [("hello", 2)])

--------------------------------------------------------------------------------
-- Type level defunctionalization
--------------------------------------------------------------------------------

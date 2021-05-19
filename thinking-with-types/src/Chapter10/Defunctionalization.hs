{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Extensions required by type-level defunctionalization section
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module Chapter10.Defunctionalization where

import Prelude hiding (fst)

-- Import required by type-level defunctionalization section
import Data.Kind (Constraint, Type)

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
type Exp a = a -> Type

type family EvalE (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

type instance EvalE (Snd '(a, b)) = b

-- Try this out!
--
-- >   λ  :set -XDataKinds
-- >   λ  :kind EvalE (Snd '(1, "hello"))
-- > EvalE (Snd '(1, "hello")) :: ghc-prim-0.5.3:GHC.Types.Symbol
-- >   λ  :kind! EvalE (Snd '(1, "hello"))
-- > EvalE (Snd '(1, "hello")) :: ghc-prim-0.5.3:GHC.Types.Symbol
-- > = "hello"

data FromMaybe :: a -> Maybe a -> Exp a
type instance EvalE (FromMaybe _1 ('Just a)) = a
type instance EvalE (FromMaybe a 'Nothing  ) = a

-- | Exercise 10.2-i: defunctionalize 'listToMaybe' at the type level.
data ListToMaybeE :: [a] -> Exp (Maybe a)
type instance EvalE (ListToMaybeE '[]) = 'Nothing
type instance EvalE (ListToMaybeE (t ': _ts)) = 'Just t

data MapListE :: (a -> Exp b) -> [a] -> Exp [b]
type instance EvalE (MapListE f '[]) = '[]
type instance EvalE (MapListE f (t ': ts)) = EvalE (f t) ': EvalE (MapListE f ts)

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Chapter9.Printf where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

-- Note the absence of a data constructor.
--
-- The kind of (:<<) is:
--
-- > k0 -> k1 -> Type
data (a :: k0) :<< (b :: k1)
infixr 5 :<<

class HasPrintf a where
  type Printf a :: Type

-- Typelevel symbol.
instance HasPrintf (text :: Symbol) where
  type Printf text = String

instance HasPrintf a => HasPrintf ((text :: Symbol) :<< a) where
  type Printf ((text :: Symbol) :<< a) = Printf a

instance HasPrintf a => HasPrintf ((param :: Type) :<< a) where
  type Printf ((param :: Type) :<< a) = param -> Printf a

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter9.Printf where

import Data.Kind (Type)
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import Data.Proxy (Proxy(Proxy))
import Data.Monoid ((<>))

-- Note the absence of a data constructor.
--
-- The kind of (:<<) is:
--
-- > k0 -> k1 -> Type
data (a :: k0) :<< (b :: k1)
infixr 5 :<<

class HasPrintf a where
  type Printf a :: Type
  format :: String
         -- ^ This is an implementation detail that acts as an accumulator
         -- where we can keep track of all of the formatting done by earlier
         -- steps.
         -> Proxy a
         -- ^ This parameter exists only to allow GHC to find the correct
         -- instance of 'HasPrintf' from the call site of 'format'.
         -> Printf a

-- Typelevel symbol.
instance KnownSymbol text => HasPrintf (text :: Symbol) where
  type Printf text = String
  format str _ = str <> symbolVal (Proxy @text)

instance (KnownSymbol text, HasPrintf a)
  => HasPrintf ((text :: Symbol) :<< a) where
  type Printf ((text :: Symbol) :<< a) = Printf a
  format str _ = format (str <> symbolVal (Proxy @text)) (Proxy @a)

instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where
  type Printf ((param :: Type) :<< a) = param -> Printf a
  format str _ = \param -> format (str <> show param) (Proxy @a)

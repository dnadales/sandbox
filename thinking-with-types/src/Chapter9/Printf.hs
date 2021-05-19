{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | To try this out in the repl you'd need to enable some extensions as
-- follows:
--
-- >   λ  :set -XDataKinds
-- >   λ  :set -XTypeOperators
-- >   λ  :set -XTypeApplications
--
-- Then you can play with this module:
--
-- >   λ  import Data.Proxy (Proxy(Proxy))
-- >   λ  printf (Proxy @"test")
-- >   λ  printf (Proxy @(Int :<< " lolo")) 1
--
-- Note that this has the limitation that you need to end the format string with some text:
--
-- >   λ  printf (Proxy @(Int :<< Bool )) 1 True
-- > <interactive>:31:1: error:
-- >     • Couldn't match type ‘Printf Bool’ with ‘Bool -> t’
-- >       Expected type: Int -> Bool -> t
-- >         Actual type: Printf (Int :<< Bool)
-- >     • The function ‘printf’ is applied to three arguments,
-- >       but its type ‘Proxy (Int :<< Bool) -> Printf (Int :<< Bool)’
-- >       has only one
-- >       In the expression: printf (Proxy @(Int :<< Bool)) 1 True
-- >       In an equation for ‘it’: it = printf (Proxy @(Int :<< Bool)) 1 True
-- >     • Relevant bindings include it :: t (bound at <interactive>:31:1)
-- >   λ  printf (Proxy @(Int :<< "Booooh" :<< Bool )) 1 True

-- > <interactive>:32:1: error:
-- >     • Couldn't match type ‘Printf Bool’ with ‘Bool -> t’
-- >       Expected type: Int -> Bool -> t
-- >         Actual type: Printf (Int :<< ("Booooh" :<< Bool))
-- >     • The function ‘printf’ is applied to three arguments,
-- >       but its type ‘Proxy (Int :<< ("Booooh" :<< Bool))
-- >                     -> Printf (Int :<< ("Booooh" :<< Bool))’
-- >       has only one
-- >       In the expression:
-- >         printf (Proxy @(Int :<< "Booooh" :<< Bool)) 1 True
-- >       In an equation for ‘it’:
-- >           it = printf (Proxy @(Int :<< "Booooh" :<< Bool)) 1 True
-- >     • Relevant bindings include it :: t (bound at <interactive>:32:1)
-- >   λ  printf (Proxy @(Int :<< Bool :<< "booooh" )) 1 True
-- > "1Truebooooh"
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

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""

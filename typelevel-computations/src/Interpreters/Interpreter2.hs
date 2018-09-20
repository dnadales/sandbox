{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- | Third version of the interpreter using injective type families.
module Interpreters.Interpreter2 where

import           Data.Functor.Identity (Identity, runIdentity)

class Interpretation i where
  type Context i = (r :: * -> *) | r -> i
  -- Instead of our original @type Context i :: * -> *@.

class Interpretation i => Interpret i a where
  type Interpreted i a :: *
  -- Same as in version 0.

  int :: a -> Context i (Interpreted i a)
  -- Same as in version 0.

data Int2Bool

instance Interpretation Int2Bool where
  type Context Int2Bool = Identity

instance Interpret Int2Bool Int where
  type Interpreted Int2Bool Int = Bool

  int :: Int -> Identity Bool
  int x = pure (x == 0)

mInt2Bool :: Int -> Bool
mInt2Bool = runIdentity . int

-- In this version, the use of injectivity does not allow us to define a second
-- instance:
--
-- > data Int2Bool2
-- >
-- > instance Interpretation Int2Bool2 where
-- >   type Context Int2Bool2 = Identity
-- >
-- > instance Interpret Int2Bool2 Int where
-- >   type Interpreted Int2Bool2 Int = Bool
-- >
-- >   int :: Int -> Identity Bool
-- >   int x = pure (x /= 0)
-- >
--
-- Declaring the instance above will give produce the following error:
--
-- >    Type family equations violate injectivity annotation:
-- >       Context Int2Bool = Identity
-- >         -- Defined at src/Interpreters/Interpreter2.hs:23:8
-- >       Context Int2Bool2 = Identity
-- >         -- Defined at src/Interpreters/Interpreter2.hs:38:8
-- >    |
-- > 23 |   type Context Int2Bool = Identity
-- >    |        ^^^^^^^
-- >

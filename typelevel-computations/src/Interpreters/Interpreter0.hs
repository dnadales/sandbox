{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
-- | An example on the use of type families, problems that might arise, and
-- injective type families. See
-- https://stackoverflow.com/questions/52413429/can-i-select-an-instance-without-resorting-to-typeapplications
module Interpreters.Interpreter0 where

import           Data.Functor.Identity (Identity, runIdentity)

class Interpretation i where
  type Context i :: * -> *

-- | An instance of this class represent an interpretation function from @a@ to
-- @Interpreted i a@, which is wrapped into some context @Context i@.
class Interpretation i => Interpret i a where
  type Interpreted i a :: *

  int :: a -> Context i (Interpreted i a)


-- * Defining an instance that interprets 'Int's into 'Bool's

-- | Here 'Int2Bool represents a tag for the instance. It allows to select the
-- right context in an interpretation instance.
data Int2Bool

instance Interpretation Int2Bool where
  type Context Int2Bool = Identity

instance Interpret Int2Bool Int where
  type Interpreted Int2Bool Int = Bool

  int :: Int -> Identity Bool
  int x = pure (x == 0)

-- | Here you need type applications in to have the compiler selecting the
-- instance above. Otherwise you'd get the error:
--
-- >     • Couldn't match type ‘Interpreted i0 Int’ with ‘Bool’
-- >       Expected type: Int -> Identity Bool
-- >         Actual type: Int -> Context i0 (Interpreted i0 Int)
-- >       The type variable ‘i0’ is ambiguous
-- >     • In the second argument of ‘(.)’, namely ‘int’
-- >       In the expression: runIdentity . int
-- >       In an equation for ‘mInt2Bool’: mInt2Bool = runIdentity . int
-- >    |
-- > 44 | mInt2Bool = runIdentity . int
-- >    |                           ^^^
-- >
--
-- Even if you explicitly specify @int :: Int -> Identity Bool@ this won't help
-- either: @Identity ~ Context i, i ~ Int2Bool@.
mInt2Bool :: Int -> Bool
mInt2Bool = runIdentity . int @Int2Bool

-- We could also can declare a second instance:
data Int2Bool2

instance Interpretation Int2Bool2 where
  type Context Int2Bool2 = Identity

instance Interpret Int2Bool2 Int where
  type Interpreted Int2Bool2 Int = Bool

  int :: Int -> Identity Bool
  int x = pure (x /= 0)

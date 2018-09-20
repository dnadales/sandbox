{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Second version of the interpreter using associated data families instead
-- of type families.
module Interpreters.Interpreter1 where

import           Data.Functor.Identity (Identity, runIdentity)

class Interpretation i where
  data Context i a
  -- Instead of 'Context i', which make the type injective.

class Interpretation i => Interpret i a where
  type Interpreted i a :: *
  -- Same as before.

  int :: a -> Context i (Interpreted i a)
  -- Same as before.

data Int2Bool

instance Interpretation Int2Bool where
  data Context Int2Bool a = I2BContext { getContext ::  Identity a }
  -- Here we have to wrap identity in a fresh constructor.
  --
  -- Note that you could use @newtype@ as well, but I used @data@ just to make
  -- things more uniform and avoid confusion while learning these concepts.
  --

instance Interpret Int2Bool Int where
  type Interpreted Int2Bool Int = Bool
  -- Same as before

  int :: Int -> Context Int2Bool Bool
  int x = I2BContext . pure $ x == 0

mInt2Bool :: Int -> Bool
mInt2Bool = runIdentity . getContext . int

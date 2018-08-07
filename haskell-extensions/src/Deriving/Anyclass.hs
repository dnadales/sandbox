{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Examples on the effects of newtype deriving.
--
module Deriving.Anyclass where

newtype Foo a = MkFoo a deriving Show

class Show a => Bar a where
    bar :: a -> String
    bar = show

deriving instance Bar a => Bar (Foo a)

instance Bar Int

-- Now try with:
--
-- >>> bar (MkFoo (22 :: Int))
-- "MkFoo 22"
--
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DeriveAnyClass
--
-- Quote:
--   With DeriveAnyClass you can derive any other class. The compiler will simply
--   generate an instance declaration with no explicitly-defined methods. This is
--   mostly useful in classes whose minimal set is empty,

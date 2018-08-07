{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- | Use of deriving strategies.
--
-- See:
-- - https://ryanglscott.github.io/2017/04/12/improvements-to-deriving-in-ghc-82/
-- - https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#default-deriving-strategy
--
module Deriving.Strategies where

newtype Foo a = MkFoo a deriving Show

class Show a => Bar a where
    bar :: a -> String
    bar = show

deriving newtype instance Bar a => Bar (Foo a)

instance Bar Int



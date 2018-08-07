{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- | Examples on the effects of newtype deriving.
module Deriving.Newtype where

newtype Foo a = MkFoo a deriving Show

class Show a => Bar a where
    bar :: a -> String
    bar = show

-- | Note how this allows you to automatically derive @Bar (Foo a)@ instances
-- for any type @a@ which is an instance of @Bar@!
--
-- For more info see: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#stand-alone-deriving-declarations
deriving instance Bar a => Bar (Foo a)

instance Bar Int

-- Now try with:
--
-- >>> bar (MkFoo (22 :: Int))
-- "22"
--
-- So it seems @GeneralizedNewtypeDeriving@ is just removing the @newtype@
-- constructor.

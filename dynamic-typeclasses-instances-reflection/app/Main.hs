{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

-- | On how to use reflection to define class instances at runtime.
--
-- Inspired by https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
module Main where

import Data.Proxy
import Data.Reflection

class FooBar a where
  foo :: a -> String
  bar :: IO ()

data FooBar_ a =
  FooBar_ { foo_ :: a -> String
          , bar_ :: IO ()
          }

newtype Dynamic a s = Dynamic { runDynamic :: a }

instance Reifies s (FooBar_ a) => FooBar (Dynamic a s) where
  foo d = foo_ (reflect d) (runDynamic d)
  bar   = bar_ (reflect (Proxy @s))

main :: IO ()
main = example
  where
    fooInt :: Int -> String
    fooInt = ("An Int with value " ++) . show

    barInt :: IO ()
    barInt = print "barInt"

    fooBool :: Bool -> String
    fooBool = ("A Bool with value " ++) . show

    barBool :: IO ()
    barBool = print "barBool"

    withFooBar
      :: forall a
       . (a -> String)
      -> IO ()
      -> (forall s . Reifies s (FooBar_ a) => Dynamic a s) -> IO ()
    withFooBar fooA barA d =
      reify (FooBar_ fooA barA) (withDynamic. asProxyOf d)
      where
        -- This is needed to make the types of 'reify' match.
        asProxyOf :: f s -> Proxy s -> f s
        asProxyOf v _ = v

        withDynamic :: forall s . Reifies s (FooBar_ a) => Dynamic a s -> IO ()
        withDynamic a = print (foo a) >> bar @(Dynamic a s)

    example :: IO ()
    example = do
      withFooBar fooInt  barInt  (Dynamic 10)
      withFooBar fooBool barBool (Dynamic True)

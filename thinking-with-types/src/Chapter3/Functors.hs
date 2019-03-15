{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter3.Functors where

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap :: (a -> b) -> T1 a -> T1 b
  fmap f (T1 fia) = T1 $ f . fia

newtype T2 a = T2 (a -> Int)

instance Functor T2 where
  fmap :: (a -> b) -> T2 a -> T2 b
  fmap f (T2 fai) = T2 $ x
    where
      x :: b -> Int
      x = undefined -- It seems we have no way of getting this function with
                    -- the arguments provided!

newtype T3 a = T3 (a -> a)

instance Functor T3 where
  fmap :: (a -> b) -> T3 a -> T3 b
  fmap f (T3 faa) = T3 x
    where
      x :: b -> b
      x = undefined -- We have the same problem here, it seems we are not able
                    -- to, given a `b` return another `b` based on what we
                    -- have.

newtype T4 a = T4 ((Int -> a) -> Int)

instance Functor T4 where
  fmap :: (a -> b) -> T4 a -> T4 b
  fmap f (T4 fgiai) = T4 x
    where
      x :: ((Int -> b) -> Int)
      x = \f -> undefined

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap :: forall a b . (a -> b) -> T5 a -> T5 b
  fmap f (T5 g) = T5 x
    where
      x :: ((b -> Int) -> Int)
      x = \fbi -> g (fbi . f)

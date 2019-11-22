{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter6.ContinuationMonad where

newtype Cont a = Cont { unCont :: forall r . (a -> r) -> r }

instance Functor Cont where

  fmap :: forall a b . (a -> b) -> Cont a -> Cont b
  fmap fab (Cont g) = Cont h
    -- fab :: a -> b
    -- g :: forall r . (a -> r) -> r
    where
      h :: forall r . (b -> r) -> r
      h fbr = fbr (g fab)

instance Applicative Cont where

  pure :: forall a . a -> Cont a
  pure a = Cont pa
    where
      pa :: forall r . (a -> r) -> r
      pa far = far a

  (<*>) :: forall a b . Cont (a -> b) -> Cont a -> Cont b
  Cont cabr <*> Cont car  = Cont cbr
  -- cabr :: forall r . ((a -> b) -> r) -> r
  -- car  :: forall r . (a        -> r) -> r
  -- ==>  :: (a        -> b) -> b
    where
      cbr :: forall r . (b -> r) -> r
      cbr fbr = fbr x
        where
          x :: b
          x = cabr car

instance Monad Cont where

  return a = pure a

  (>>=) :: forall a b . Cont a -> (a -> Cont b) -> Cont b
  Cont car >>= faContbr = car faContbr -- Cont cbr
    --   car :: forall r . (a -> r) -> r

    --   faContbr :: a -> Cont b -- forall r . (b -> r) -> r

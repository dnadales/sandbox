{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Chapter15.Chapter15 where

import           Data.Kind     (Type)
import           Data.Typeable ()
import           Data.Void     ()
import           Unsafe.Coerce (unsafeCoerce)

data family Sing (a :: k)

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing
  :: SomeSing k
  -> (forall (a :: k). Sing a -> r)
  -> r
withSomeSing (SomeSing singVal) f = f singVal

class SingKind k where
  type Demote k = r | r -> k
  toSing   :: Demote k      -> SomeSing k
  fromSing :: Sing (a :: k) -> Demote k -- Note that k is used both as a kind and as a type!

class SingI (a :: k) where
  sing :: Sing a

--------------------------------------------------------------------------------
-- Singletons instances for Bool
--------------------------------------------------------------------------------

data instance Sing (a :: Bool) where
  STrue  :: Sing 'True
  SFalse :: Sing 'False

instance SingKind Bool where
  type Demote Bool = Bool

  -- toSing :: Bool -> SomeSing Bool
  toSing True  = SomeSing STrue
  toSing False = SomeSing SFalse -- Note that this compiles without a problem!
  -- toSing False = SomeSing STrue

  -- fromSing :: Sing Bool -> Bool
  fromSing STrue  = True
  fromSing SFalse = False

instance SingI 'True where
  sing = STrue

instance SingI 'False where
  sing = SFalse


-- Try this in the repl!
--
-- withSomeSing (toSing True) fromSing
--
-- withSomeSing (toSing False) fromSing
--
-- withSomeSing (toSing False) fromSing == False

--------------------------------------------------------------------------------
-- Singletons instances for Maybe
--------------------------------------------------------------------------------

data instance Sing (a :: Maybe k) where
  SNothing :: Sing 'Nothing
  SJust    :: Sing (a :: k) -> Sing ('Just a)
  -- SJust    :: a  -> Sing ('Just a) -- This compiles as well, but it's probably incorrect.

instance SingI 'Nothing where
  sing = SNothing

instance SingI a => SingI ('Just a) where
  sing = SJust sing

instance ( k ~ Demote k
         , SingKind k
         )
         => SingKind (Maybe k) where
  type Demote (Maybe k) = Maybe k

  -- toSing :: Maybe a -> SomeSing (Maybe a)
  toSing Nothing  = SomeSing SNothing
  toSing (Just a) =
    withSomeSing (toSing a) $ SomeSing . SJust

  -- fromSing :: Sing (a :: Maybe k) -> Maybe k
  fromSing SNothing       = Nothing
  fromSing (SJust sing_a) = Just $ fromSing sing_a

--------------------------------------------------------------------------------
-- Singletons instance for Lists
--------------------------------------------------------------------------------

data instance Sing (a :: [k]) where
  SNil  :: Sing '[]
  SCons :: Sing (h :: k) -> Sing (t :: [k]) -> Sing (h ': t)

instance SingI '[] where
  sing = SNil

instance (SingI h, SingI t) => SingI (h ': t) where
  sing = SCons sing sing

instance (k ~ Demote k, SingKind k) => SingKind [k] where
  type Demote [k] = [k]

  -- toSing :: [k] -> SomeSing [k]
  toSing []       = SomeSing SNil
  -- You can work this backwards: you know you need to return some a value of
  -- type 'SomeSing', which requires you to use the 'SomeSing' constructor,
  -- which takes a value of type 'Sing [k]'.
  --
  -- How can we get 'sing_h' and 'sing_t' in:
  --
  -- SCons sing_h sing_t
  --
  -- Function 'withSomeSing' allow us to get a singleton value from 'SomeSing'
  toSing (x : xs) =
    withSomeSing (toSing x) $ \sing_h ->
      withSomeSing (toSing xs) $ \sing_t ->
        SomeSing $ SCons sing_h sing_t

  fromSing SNil         = []
  fromSing (SCons x xs) = fromSing x : fromSing xs

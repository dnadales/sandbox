{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Chapter13.Chapter13 where

import           GHC.Generics

class ExNihilo a where
  gExNihilo :: Maybe (a x)

instance ExNihilo U1 where
  gExNihilo = Just U1

instance ExNihilo (K1 _1 a) where
  gExNihilo = Nothing

instance ExNihilo (a :+: b) where
  gExNihilo = Nothing

instance ExNihilo (a :*: b) where
  gExNihilo = Nothing

instance ExNihilo a => ExNihilo (M1 x y a) where
  gExNihilo = fmap M1 gExNihilo

exNihilo :: (Generic a, ExNihilo (Rep a)) => Maybe a
exNihilo = fmap to gExNihilo

data Foo = Foo
  deriving (Generic, Show)

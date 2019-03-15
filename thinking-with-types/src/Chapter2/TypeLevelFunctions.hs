{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter2.TypeLevelFunctions where

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or 'False y = 'False

type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

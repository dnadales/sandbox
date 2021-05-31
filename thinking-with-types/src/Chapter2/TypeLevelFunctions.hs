{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Chapter2.TypeLevelFunctions where

import Data.Proxy (Proxy)

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or 'False y = 'False

type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

-- We cannot write
--
-- > f :: '[ 'True] -> String
--
-- Writing this will result in the following error:
--
--     • Expected a type, but ‘'[ 'True]’ has kind ‘[Bool]’
--     • In the type signature: f :: '[ 'True] -> String
--    |
-- 14 | f :: '[ 'True] -> String
--    |      ^^^^^^^^^
--
-- So '->' is a function from types to types, whereas '[ 'True] has kind
-- [Bool]:
--
-- >>> :kind (->)
-- (->) :: * -> * -> *
--
-- >>> :kind '[ 'True]
-- '[ 'True] :: [Bool]
--
-- >>> :kind Proxy
-- Proxy :: k -> *
--
-- >>> :kind Proxy '[ 'True]
-- Proxy '[ 'True] :: *
--
f :: Proxy '[ 'True] -> String
f _ = "all true!"


-- A note on PolyKinds. I you enable the PolyKinds extension then the following
-- data type (MyProxy) will get the k -> * type, instead of * -> *. Try this out!
--
-- >>> :kind MyProxy '[ 'False]
--
-- <interactive>:1:9: error:
--   • Expected a type, but ‘'[ 'False]’ has kind ‘[Bool]’
--   • In the first argument of ‘MyProxy’, namely ‘'[ 'False]’
--     In the type ‘MyProxy '[ 'False]’
--
-- Enable the extension ...
-- >>> :r
-- [8 of 8] Compiling Chapter2.TypeLevelFunctions ( /home/damian/github/dnadales/sandbox/thinking-with-types/src/Chapter2/TypeLevelFunctions.hs, interpreted )
-- Ok, 8 modules loaded.
-- >>> :kind MyProxy '[ 'False]
-- MyProxy '[ 'False] :: *
data MyProxy t = MyProxy

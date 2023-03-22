{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Lib where

import           Data.SOP
import           Data.SOP.NP

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--------------------------------------------------------------------------------
-- Era tags
--------------------------------------------------------------------------------
data Byron
data Shelley
data Allegra

data family Version era

--------------------------------------------------------------------------------
-- Byron era definitnions
--------------------------------------------------------------------------------

data instance Version Byron = ByronVersion

--------------------------------------------------------------------------------
-- Shelley era definitnions
--------------------------------------------------------------------------------

data instance Version Shelley = ShelleyVersion

--------------------------------------------------------------------------------
-- Allegra era definitnions
--------------------------------------------------------------------------------

data instance Version Allegra = AllegraVersion

--------------------------------------------------------------------------------
-- MyShow
--------------------------------------------------------------------------------

class MyShow a where
  myShow :: a -> String

instance MyShow (Version Byron) where
  myShow _ = "Version Byron"

instance MyShow (Version Shelley) where
  myShow _ = "Version Shelley"

instance MyShow (Version Allegra) where
  myShow _ = "Version Allegra"

--------------------------------------------------------------------------------
-- Cardano
--------------------------------------------------------------------------------

cardanoVersions :: NP Version [Byron, Shelley, Allegra]
cardanoVersions = ByronVersion :* ShelleyVersion :* AllegraVersion :* Nil

-- showCardanoVersions :: [String]
-- showCardanoVersions = hcollapse npCardanoVersions
--   where
--      npCardanoVersions :: NP (K String) [Byron, Shelley, Allegra]
--      npCardanoVersions = hcmap (Proxy @(MyShow `Compose` Version))
--                                (K . myShow)
--                                 cardanoVersions
showCardanoVersions :: [String]
showCardanoVersions = showErasVersions cardanoVersions

--------------------------------------------------------------------------------
-- Generalization
--------------------------------------------------------------------------------

showErasVersions ::
    forall xs
   . (All (Compose MyShow Version) xs)
  => NP Version xs -> [String]
showErasVersions versions = hcollapse npVersions
  where
     npVersions :: NP (K String) xs
     npVersions = hcmap (Proxy @(MyShow `Compose` Version))
                        (K . myShow)
                        versions

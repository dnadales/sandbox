{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Lib where

import           Data.Kind   (Type)
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

data family ShelleyBased era :: Type

data instance Version (ShelleyBased era) = ShelleyVersion (EraSpecific era)

type family EraSpecific era :: Type

data ShelleySpecific = ShelleySpecific

type instance EraSpecific Shelley = ShelleySpecific


--------------------------------------------------------------------------------
-- Allegra era definitnions
--------------------------------------------------------------------------------

data AllegraSpecific = AllegraSpecific

type instance EraSpecific Allegra = AllegraSpecific

--------------------------------------------------------------------------------
-- MyShow
--------------------------------------------------------------------------------

class MyShow a where
  myShow :: a -> String

instance MyShow (Version Byron) where
  myShow _ = "Version Byron"

instance MyShow (Version (ShelleyBased Shelley)) where
  myShow _ = "Version Shelley"

instance MyShow (Version (ShelleyBased Allegra)) where
  myShow _ = "Version Allegra"

--------------------------------------------------------------------------------
-- Cardano
--------------------------------------------------------------------------------

cardanoVersions :: NP Version [Byron, ShelleyBased Shelley, ShelleyBased Allegra]
cardanoVersions =  ByronVersion
                :* ShelleyVersion ShelleySpecific
                :* ShelleyVersion AllegraSpecific
                :* Nil

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

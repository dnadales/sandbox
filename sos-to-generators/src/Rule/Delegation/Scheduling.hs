{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Rule.Delegation.Scheduling where

import           Control.Lens             (makeLenses, (%~))
import           Control.Monad.State      (State)
import           Data.Function            ((&))
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Numeric.Natural          (Natural)
import           QuickCheck.GenT          (GenT, elements, liftGen, listOf)
import           Test.QuickCheck          (Gen)

import           Control.State.Transition

newtype Epoch = Epoch Natural -- TODO: should be a natural number
  deriving (Show, Eq, Ord, Num)

newtype Slot = Slot Natural
  deriving (Show, Eq, Ord, Num)

-- |Representation of the owner of key pair.
newtype Owner = Owner Natural deriving (Show, Eq, Ord)

-- |Signing Key.
newtype SKey = SKey Owner deriving (Show, Eq, Ord)

-- |Verification Key.
newtype VKey = VKey Owner deriving (Show, Eq, Ord)

newtype VKeyGen = VKeyGen VKey
  deriving (Show, Eq, Ord)

data Cert
  = Cert { depoch :: Epoch
         , dwho   :: (VKeyGen, VKey)
         }
  deriving (Show, Eq)

-- | Delegation scheduling environment
data DSEnv
  = DSEnv { k :: Set VKeyGen
          , e :: Epoch
          , s :: Slot
          , d :: Slot -- TODO: replace this by `SlotCount`
          }

-- | Delegation scheduling state
data DSState
  = DSState { _sds :: [(Slot, (VKeyGen, VKey))]
            , _eks :: Set (Epoch, VKeyGen)
            }
makeLenses ''DSState


--------------------------------------------------------------------------------
-- SDELEG transition system
--------------------------------------------------------------------------------

-- | Tag for the SDELEG transition system
data SDELEG

instance STS SDELEG where
  type Environment SDELEG = DSEnv

  type State SDELEG = DSState

  type Signal SDELEG = Cert

  data PredicateFailure SDELEG
    = DelegPastEpoch { currEpoch :: Epoch, givenEpoch :: Epoch }
    deriving (Show)

  rules =
    [ Rule [Predicate $ \env _st cert ->
             e env <. depoch cert]
           (Extension . Transition $ \env st cert ->
             st & (eks %~ Set.insert (depoch cert, fst . dwho $ cert))
                . (sds %~ ((s env + d env, dwho cert):))
           )
    ]
    where
      e0 <. e1
        | e0 < e1 = Passed
        | otherwise = Failed (DelegPastEpoch e0 e1)

-- |
nextState :: DSState -> DSEnv -> DSState
nextState = undefined

sdelegTG :: DSEnv -> DSState -> Gen (Cert, DSState)
sdelegTG env st = undefined
  -- .. generate some stuff
  -- .. apply the transition to validate the `Cert` and get `DSState`!
-- do
--   vk_s <- oneOf (k env)
--   e_d <- largerThan e
--   (e_d, vk_s) `notIn` (eks st)


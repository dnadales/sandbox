{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Rule.Delegation.Scheduling where

import           Control.Lens             (makeLenses, (%~), (^.))
import           Data.Either              (partitionEithers)
import           Data.Foldable            (traverse_)
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
    [RuleExtension $ \env st cert ->  do
        e env <. depoch cert
        return $
          st & (eks %~ Set.insert (depoch cert, fst . dwho $ cert))
             . (sds %~ ((s env + d env, dwho cert):))
    ]
    where
      e0 <. e1
        | e0 < e1 = Right ()
        | otherwise = Left (DelegPastEpoch e0 e1)


    -- [ Rule [Predicate $ \env _st cert ->
    --          e env <. depoch cert]
    --        (Extension . Transition $ \env st cert ->
    --          st & (eks %~ Set.insert (depoch cert, fst . dwho $ cert))
    --             . (sds %~ ((s env + d env, dwho cert):))
    --        )
    -- ]

-- | Compute the next state of an STS.
nextState
  :: forall sts . STS sts
  => Environment sts
  -> State sts
  -> Signal sts
  -> State sts
nextState env st s = undefined
-- res
--   where
--     -- TODO: we have to decide what to do if no rule apply. Since we're testing
--     -- the generator, we might want this to fail (with an appropriate error
--     -- message).
--     (_, res:_) = partitionEithers (nextStateVia <$> rules)
--     nextStateVia :: Rule sts -> Either (PredicateFailure sts) (State sts)
--     nextStateVia = undefined
--     -- nextStateVia (Rule antecedents consequent) =
--     --   traverse_ holds antecedents >> return (apply consequent)
--     holds :: Antecedent sts -> Either (PredicateFailure sts) ()
--     holds (SubTrans subEnv stGet rule) = undefined
--     apply :: Consequent sts -> State sts
--     apply = undefined


-- To apply the generators we could have a function like:

-- | Generate a signal that satisfies the given rule.
sigGen
  :: STS sts
  => Rule sts
  -> Environment sts
  -> State sts
  -> Gen (Signal sts)
sigGen = undefined

--sdelegTG :: DSEnv -> DSState -> Gen Cert -- Was 'Gen (Cert, DSState)' we don't need to generate the state randomly
-- sdelegTG env st = undefined
  -- .. generate some stuff

  -- .. tricky part: how do we call the generator of a rule in the premise?

  -- .. apply the transition to validate the `Cert` and get `DSState`!


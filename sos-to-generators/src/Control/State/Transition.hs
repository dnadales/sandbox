-- Taken from https://github.com/input-output-hk/cardano-chain. Credit: Nicholas Clarke
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Small step state transition systems.
module Control.State.Transition where

import           Control.Lens

-- | State transition system.
class (Show (PredicateFailure a))
  => STS a where
  -- | Type of the state which the system transitions between.
  type State a :: *
  -- | Signal triggering a state change.
  type Signal a :: *
  -- | Environment type.
  type Environment a :: *

  -- | Descriptive type for the possible failures which might cause a transition
  -- to fail.
  data PredicateFailure a :: *

  -- | Rules governing transition under this system.
  rules :: [Rule a]

-- | Rules generating initial states
initialRules
  :: STS a
  => [Rule a]
initialRules = filter isInitial rules
  where
    isInitial (RuleBase _)      = True
    isInitial (RuleExtension _) = False

-- | The union of the components of the system available for making judgments.
type JudgmentContext sts = (Environment sts, State sts, Signal sts)

-- | A transition under a state transition system
data Transition sts where
  Transition
    :: (   Environment sts
        -> State sts
        -> Signal sts
        -> Either (PredicateFailure sts) (State sts)
       )
    -> Transition sts

-- | Apply a transition
transition
  :: Transition sts
  -> Environment sts
  -> State sts
  -> Signal sts
  -> Either (PredicateFailure sts) (State sts)
transition (Transition f) = f

-- | Embed one STS within another.
class (STS sub, STS super) => Embed sub super where
  envLens :: Lens' (Environment super) (Environment sub)

  -- | Extract the state of the subsystem as a component of the super-system.
  stateLens :: Lens' (State super) (State sub)

data PredicateResult sts
  = Passed
  | Failed (PredicateFailure sts)

-- | A rule within a transition system.
data Rule sts where
  RuleBase
    :: (  Environment sts
        -> Either (PredicateFailure sts) (State sts)
       )
    -> Rule sts

  RuleExtension
    :: (   Environment sts
        -> State sts
        -> Signal sts
        -> Either (PredicateFailure sts) (State sts)
       )
    -> Rule sts

--------------------------------------------------------------------------------
-- Testing state validity
--------------------------------------------------------------------------------

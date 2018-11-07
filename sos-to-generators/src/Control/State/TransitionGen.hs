{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module Control.State.TransitionGen where

import           Control.Lens              (makeLenses, (%~), (^.))
import           Control.Monad             (mplus, mzero)
import           Control.Monad.Except      (ExceptT (ExceptT), lift, runExceptT)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.Maybe                (catMaybes)
import           Test.QuickCheck           (Gen, choose, elements, frequency,
                                            oneof)

-- | Every transition rule defines a signal generator. If in the given state
-- and environment no transition is possible (i.e. there exists no signal that
-- triggers a valid rule), then the generator can return @Nothing@.
type SigGen env st sig
  = env -> st -> MaybeT Gen (sig, st)

-- | Successful trace.
data Trace st sig
  = Trace
    { _traceTrans     :: [(st, sig)]
    -- ^ Trace's transitions. Newest first.
    , _traceInitState :: st
    -- ^ At trace contains at least one initial state.
    }
  deriving (Eq, Show)

makeLenses ''Trace

-- | Signals in the trace. The newest signal is at the beginning.
traceSigs :: Trace st sig -> [sig]
traceSigs = fmap snd . _traceTrans

-- | States in the trace. The newest state is at the beginning.
traceStates :: Trace st sig -> [st]
traceStates tr = fmap fst (tr ^. traceTrans) ++ [tr ^. traceInitState]

-- | Failing trace. We consider minimal failing traces, which must have a
-- non-failing prefix.
data FTrace st sig failure
  = FTrace
  { _tracePrefix  :: Trace st sig
  , _traceFailure :: failure
  }
  deriving (Eq, Show)

-- | A generator for traces.
sigsGen
  :: forall env st sig
  .  env
  -- ^ Environment for the generator.
  -> st
  -- ^ Initial state.
  -> [SigGen env st sig]
  -- ^ Set of transition rules (encoded as signal generators).
  -> Gen (Trace st sig)
sigsGen env st gs = fmap (`Trace` st)  (go st [])
  where
    go
      :: st
      -- ^ State before applying the signal
      -> [(st, sig)]
      -- ^ Accumulation of generated signals and states after applying these
      -- signal.
      -> Gen [(st, sig)]
      -- ^ New generated signal and state after applying this signal.
    go stPre acc = do
      mSigSt <- nextSig stPre
      case mSigSt of
        Nothing ->
          return acc
        Just (sig, stNext) ->
          -- NOTE: The numbers 5 and 95 control the probability of generating
          -- long traces. These could be tweaked by adding them as parameters.
          frequency [ (5, return acc)
                    , (95, go stNext ((stNext, sig):acc))
                    ]

    -- | Generate the next signal using the given set of transition rules
    -- ('gs')
    nextSig :: st -> Gen (Maybe (sig, st))
    nextSig stCurr =
      runMaybeT (apply env stCurr gs)

-- | Try and apply one of the rules (non-deterministically).
apply
  :: env
  -> st
  -> [SigGen env st sig]
  -> MaybeT Gen (sig, st)
apply env st rs = tryApply (env, st) (fmap uncurry rs)

-- | Try to apply one of the generator functions. If this function fails try
-- with another. Functions are chosen in an arbitrary order. If no function
-- succeeds this function returns 'mzero'.
tryApply
  :: a
  -> [a -> MaybeT Gen r]
  -> MaybeT Gen r
tryApply _ [] = mzero
tryApply a rs = do
  i <- lift $ choose (0, length rs - 1)
  (rs !! i) a `mplus` tryApply a (take i rs ++ drop (i+1) rs)

-- | A signal generator that accepts extra information for generating the
-- signal. Here 'preSig' is intended to be a pre-filled version of 'sig'. The
-- idea is that the generator will try to generate only those parts of 'sig'
-- that are not present in 'preSig'. In one extreme, 'preSig' will contain no
-- information from 'sig', so all the components in 'sig' will be generated. On
-- the other extreme, all the components of 'sig' will be predetermined by
-- 'preSig', so the generator will not generate any random data and will only
-- check that 'preSig = sig' fulfills all the conditions for the transition to
-- take place, and apply the signal to the pre-state to obtain the post state.
type PreSigGen env st preSig sig
  = env -> st -> preSig -> MaybeT Gen (sig, st)

-- | A signal 'preSig' is a pre-signal for 'sig'.
class PreSig preSig sig where
  -- | A 'preSig' without any pre-filled information.
  any :: preSig

  -- | Convert a signal to a 'pre-filled' signal (where all its fields will be
  -- determined).
  asPreSig :: sig -> preSig

instance PreSig preSig sig => PreSig [preSig] [sig] where
  any = []

  asPreSig = fmap asPreSig

-- | A version of apply that works with 'PreSigGen''s.
preApply
  :: env
  -> st
  -> preSig
  -> [PreSigGen env st preSig sig]
  -> MaybeT Gen (sig, st)
-- We might want to remove SigGen in favor of PreSigGen. But I'm leaving it for
-- now so that we can compare the different ways in which data flows from one
-- rule to the next.
preApply env st preSig rs = tryApply (env, st, preSig) (fmap uncurry3 rs)
  where uncurry3 f (a, b, c) = f a b c

-- | Shrink function on traces. Given a trace @traceShrink tr@ will return the
-- sub-traces of 'tr' in increasing length (starting with the trace that
-- contains only the initial state of 'tr').
traceShrink :: Trace st sig -> [Trace st sig]
traceShrink (Trace [] _)
  = [] -- Nothing left to shrink.
traceShrink (Trace (_:xs) st)
    -- The most aggressive shrinking should go at the beginning, so that the
    -- property can be checked with the smallest trace possible. That is why
    -- `prevTrace` is put at the end.
    --
    -- TODO: make this O(n).
    = traceShrink prevTrace ++ [prevTrace]
    where
      prevTrace = Trace xs st


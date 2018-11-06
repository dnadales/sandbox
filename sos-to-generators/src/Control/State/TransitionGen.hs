{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module Control.State.TransitionGen where

import           Control.Lens              (makeLenses, (%~), (^.))
import           Control.Monad.Except      (ExceptT (ExceptT), lift, runExceptT)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.Maybe                (catMaybes)
import           Test.QuickCheck           (Gen, elements, frequency, oneof)

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
    }
  deriving (Eq, Show)

makeLenses ''Trace

traceSigs :: Trace st sig -> [sig]
traceSigs = fmap snd . _traceTrans

traceStates :: Trace st sig -> [st]
traceStates tr = fmap fst (tr ^. traceTrans) ++ [tr ^. traceInitState]

-- | Failing trace. We consider minimal failing traces, which must have a
-- non-failing prefix.
data FTrace st sig failure
  = FTrace
  { _tracePrefix  :: Trace st sig
  , _traceFailure :: failure
  }
  deriving Show

sigsGen
  :: forall env st sig failure
  .  env
  -> st
  -> [SigGen env st sig]
  -> Gen (Trace st sig)
sigsGen env st gs = go [] st
  where
    go :: [(st, sig)] -> st -> Gen (Trace st sig)
    go acc stPrev = do
      mSigSt <- nextSig stPrev
      case mSigSt of
        Nothing ->
          return $ Trace acc stPrev
        Just (sig, stNext) ->
          frequency [ (5, return $ Trace acc stPrev)
                    , (95, go ((stPrev, sig):acc) stNext)
                    ]

    nextSig :: st -> Gen (Maybe (sig, st))
    nextSig stCurr =
      runMaybeT (apply env stCurr gs)

-- | Try and apply one of the rules (non-deterministically).
apply
  :: env
  -> st
  -> [SigGen env st sig]
  -> MaybeT Gen (sig, st)
apply env st rs = do
  sigStates <- lift $ traverse (\r -> runMaybeT (r env st)) rs
  case catMaybes sigStates of
    [] -> -- No rules could be applied.
      MaybeT (return Nothing)
    xs -> -- Pick one of the resulting states.
      lift (elements xs)

-- | A signal generator that accepts extra information for generating the
-- signal.
type PreSigGen env st preSig sig
  = env -> st -> preSig -> MaybeT Gen (sig, st)

class PreSig preSig sig where
  any :: preSig

  asPreSig :: sig -> preSig

instance PreSig preSig sig => PreSig [preSig] [sig] where
  any = []

  asPreSig = fmap asPreSig

preApply
  :: env
  -> st
  -> preSig
  -> [PreSigGen env st preSig sig]
  -> MaybeT Gen (sig, st)
preApply = undefined

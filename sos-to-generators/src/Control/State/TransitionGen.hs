{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Control.State.TransitionGen where

import           Control.Lens    (makeLenses, (%~), (^.))
import           Test.QuickCheck (Gen, elements, frequency)

type SigGen env st sig failure
  = env -> st -> Gen (Either failure (sig, st))

data Trace st sig failure
  = Trace { _traceHead :: Either failure (sig, st)
          , _traceTail :: [(st, sig)]
          -- ^ Trace's tail: the newest states are placed first.
          }
  deriving (Eq, Show)

makeLenses ''Trace

traceSigs :: Trace st sig failure -> [sig]
traceSigs = fmap snd . _traceTail

traceStates :: Trace st sig failure -> [st]
traceStates = fmap fst . _traceTail

sigsGen
  :: forall env st sig failure
  .  env
  -> st
  -> [SigGen env st sig failure]
  -> Gen (Trace st sig failure)
sigsGen env st gs = go [] st
  where
    go :: [(st, sig)] -> st -> Gen (Trace st sig failure)
    go acc stPrev = do
      eSig <- nextSig stPrev
      case eSig of
        Left _ ->
          return $ Trace eSig acc
        Right (sig, stNext) ->
          frequency [ (5, return $ Trace eSig acc)
                    , (95, go ((stPrev, sig):acc) stNext)
                    ]

    nextSig :: st -> Gen (Either failure (sig, st))
    nextSig stCurr = traverse (\f -> f env stCurr) gs >>= elements


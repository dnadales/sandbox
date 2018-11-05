{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.State.TransitionGen where

import           Test.QuickCheck (Gen, elements, frequency)

type SigGen env st sig failure
  = env -> st -> Gen (Either failure (sig, st))

sigsGen
  :: forall env st sig failure
  .  env
  -> st
  -> [SigGen env st sig failure]
  -> Gen ([(st, sig)], Either failure (sig, st))
sigsGen env st gs = go [] st
  where
    go :: [(st, sig)] -> st -> Gen ([(st, sig)], Either failure (sig, st))
    go acc stPrev = do
      eSig <- nextSig stPrev
      case eSig of
        Left _ ->
          return (acc, eSig)
        Right (sig, stNext) ->
          frequency [ (5, return (acc, eSig))
                    , (95, go ((stPrev, sig):acc) stNext)
                    ]

    nextSig :: st -> Gen (Either failure (sig, st))
    nextSig stCurr = traverse (\f -> f env stCurr) gs >>= elements

sigs :: [(st, sig)] -> [sig]
sigs = fmap snd

states :: [(st, sig)] -> [st]
states = fmap fst


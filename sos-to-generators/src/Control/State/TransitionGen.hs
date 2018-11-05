{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.State.TransitionGen where

import           Test.QuickCheck (Gen, elements, oneof)

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
      eSig <- nextSig
      case eSig of
        Left _ ->
          return (acc, eSig)
        Right (sig, stNext) ->
          oneof [ return (acc, eSig)             -- TODO: we might want to use @frequency@.
                , go ((stPrev, sig):acc) stNext
                ]

    nextSig :: Gen (Either failure (sig, st))
    nextSig = traverse (\f -> f env st) gs >>= elements


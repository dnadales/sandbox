{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Control.State.TransitionGen where

import           Control.Lens         (makeLenses, (%~), (^.))
import           Control.Monad.Except (ExceptT (ExceptT), lift, runExceptT)
import           Test.QuickCheck      (Gen, elements, frequency, oneof)

type SigGen env st sig failure
--  = env -> st -> Gen (Either failure (sig, st))
  = env -> st -> ExceptT failure Gen (sig, st)

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
    nextSig stCurr = runExceptT (apply env stCurr gs)

-- | Try and apply the rules in sequence till one can be applied.
apply :: env -> st -> [SigGen env st sig failure] -> ExceptT failure Gen (sig, st)
apply _ _ [] = lift (elements [])
apply env st [g] =
  g env st
apply env st (g:gs) = do
  let gRes = runExceptT (g env st)
  res <- lift gRes
  case res of
    Left _              -> apply env st gs
    Right (sig, stNext) -> return (sig, stNext)

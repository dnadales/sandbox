{-# LANGUAGE TemplateHaskell #-}
-- | Rules for the delegation interface.

module Rule.Delegation.Interface where

import           Control.Lens                (makeLenses, (.~), (^.))
import           Data.Function               ((&))
import qualified Data.Set                    as Set

import           Control.State.TransitionGen
import           Rule.Common
import           Rule.Delegation.Activation
import           Rule.Delegation.Scheduling

type DIEnv = DSEnv

data DIState
  = DIState
    { _scheduling :: DSState
    , _activation :: DState}
  deriving (Show, Eq)

makeLenses ''DIState

-- | The type of `delegCompose` means that we cannot "push" data to it
delegCompose :: SigGen DIEnv DIState [DCert]
delegCompose env st = do
  (cs, st' ) <-  apply env (st ^. scheduling) sdelegs
  let active :: [PDSig]
      active = [asPreSig d | d@(s', _) <- st' ^. sds
                           , s' <= s env ]
  (_, st'') <- preApply () (st ^. activation) active adelegs
  let
    sds' = [ sd | sd@(s', _) <- st' ^. sds
                , s env - d env <= s'
                , s' <= s env + d env ]
    eks' = Set.filter ((<= e env) . fst) (st' ^. eks)
    nextSt = st & (activation .~ st'')
                . (scheduling . sds .~ sds')
                . (scheduling . eks .~ eks')
  return (cs, nextSt)

deleg = [delegCompose]

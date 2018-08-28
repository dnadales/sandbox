{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Spec for echo.

module EchoSpec (spec) where

import           GHC.Generics                  (Generic, Generic1)

import           Data.TreeDiff                 (ToExpr)
import           Test.Hspec                    (Spec, it, pending)
import           Test.QuickCheck               (Property, (===))
import           Test.QuickCheck.Monadic       (monadicIO)
import           Test.StateMachine             (Concrete, Reason (Ok),
                                                StateMachine, checkCommandNames,
                                                forAllCommands, prettyCommands,
                                                runCommands)
import qualified Test.StateMachine.Types.Rank2 as Rank2

spec :: Spec
spec = it "Right echo implementation" pending

prop_echoOK :: Property
prop_echoOK = forAllCommands echoSM Nothing $ \cmds -> monadicIO $ do
    (hist, _, res) <- runCommands echoSM cmds
    prettyCommands echoSM hist (res === Ok)

echoSM :: StateMachine Model Action IO Response
echoSM = undefined

deriving instance ToExpr (Model Concrete)

-- | The model holds the last string that was input.
newtype Model (r :: * -> *) = Model (Maybe String)
  deriving (Eq, Show, Generic)

-- | The system supports only a single type of input actions, which contain the
-- string to be echoed.
data Action (r :: * -> *) = In String
  deriving (Show, Generic1, Rank2.Foldable, Rank2.Traversable, Rank2.Functor)

-- | The system gives a single type of output response, containing a string
-- with the input previously received.
data Response (r :: * -> *) = Out String
  deriving (Show, Generic1, Rank2.Foldable, Rank2.Traversable, Rank2.Functor)

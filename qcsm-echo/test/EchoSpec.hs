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
import           Test.QuickCheck               (Gen, Property, arbitrary,
                                                ioProperty, oneof, (===))
import           Test.QuickCheck.Monadic       (monadicIO)
import           Test.StateMachine             (Concrete, GenSym,
                                                Logic (Bot, Top), Reason (Ok),
                                                StateMachine (StateMachine),
                                                Symbolic, checkCommandNames,
                                                forAllCommands, prettyCommands,
                                                runCommands, (.==))
import           Test.StateMachine.Types       (distribution, generator,
                                                initModel, invariant, mock,
                                                postcondition, precondition,
                                                runner, semantics, shrinker,
                                                spostcondition, transition)
import qualified Test.StateMachine.Types.Rank2 as Rank2

import           Echo                          (Env, input, mkEnv, output)

spec :: Spec
spec = it "Right echo implementation" $ ioProperty $ do
    env <- mkEnv
    return (prop_echoOK env)

prop_echoOK :: Env -> Property
prop_echoOK env = forAllCommands echoSM' Nothing $ \cmds -> monadicIO $ do
    (hist, _, res) <- runCommands echoSM' cmds
    prettyCommands echoSM' hist (res === Ok)
    where echoSM' = echoSM env

echoSM :: Env -> StateMachine Model Action IO Response
echoSM env = StateMachine
    { initModel = Empty
    -- ^ At the beginning of time nothing was received.
    , transition = mTransitions
    , precondition = mPreconditions
    , postcondition = mPostconditions
    , spostcondition = Nothing
    , generator = mGenerator
    , invariant = Nothing
    , distribution = Nothing
    , shrinker = mShrinker
    , semantics = mSemantics
    , mock = mMock
    , runner = id
    }
    where
      mTransitions :: Model r -> Action r -> Response r -> Model r
      mTransitions Empty   (In str) _   = Buf str
      mTransitions (Buf _) Echo     _   = Empty
      mTransitions Empty   Echo     _   = Empty
      -- TODO: qcsm will match the case below. However we don't expect this to happen!
      mTransitions (Buf str) (In _)   _ = Buf str -- Dummy response
          -- error "This shouldn't happen: input transition with full buffer"

      -- | There are no preconditions for this model.
      mPreconditions :: Model Symbolic -> Action Symbolic -> Logic
      mPreconditions _ _ = Top

      -- | Post conditions for the system.
      mPostconditions :: Model Concrete -> Action Concrete -> Response Concrete -> Logic
      mPostconditions Empty     (In _) InAck     = Top
      mPostconditions (Buf _)   (In _) ErrFull   = Top
      mPostconditions _         (In _) _         = Bot
      mPostconditions Empty     Echo   ErrEmpty  = Top
      mPostconditions Empty     Echo   _         = Bot
      mPostconditions (Buf str) Echo   (Out out) = str .== out
      mPostconditions (Buf _)   Echo   _         = Bot

      -- | Generator for symbolic actions.
      mGenerator :: Model Symbolic -> Gen (Action Symbolic)
      mGenerator _ =  oneof
          [ In <$> arbitrary
          , return Echo
          ]

      -- | Trivial shrinker.
      mShrinker :: Action Symbolic -> [Action Symbolic]
      mShrinker _ = []

      -- | Here we'd do the dispatch to the actual SUT.
      mSemantics :: Action Concrete -> IO (Response Concrete)
      mSemantics (In str) = do
          success <- input env str
          return $ if success
                   then InAck
                   else ErrFull
      mSemantics Echo = maybe ErrEmpty Out <$> output env

      -- | What is the mock for?
      mMock :: Model Symbolic -> Action Symbolic -> GenSym (Response Symbolic)
      mMock Empty (In _)   = return InAck
      mMock (Buf _) (In _) = return ErrFull
      mMock Empty Echo     = return ErrEmpty
      mMock (Buf str) Echo = return (Out str)

deriving instance ToExpr (Model Concrete)

-- | The model contains the last string that was communicated in an input
-- action.
data Model (r :: * -> *)
    = -- | The model hasn't been initialized.
      Empty
    | -- | Last input string (a buffer with size one).
      Buf String
  deriving (Eq, Show, Generic)

-- | Actions supported by the system.
data Action (r :: * -> *)
    = -- | Input a string, which should be echoed later.
      In String
      -- | Request a string output.
    | Echo
  deriving (Show, Generic1, Rank2.Foldable, Rank2.Traversable, Rank2.Functor)

-- | The system gives a single type of output response, containing a string
-- with the input previously received.
data Response (r :: * -> *)
    = -- | Input acknowledgment.
      InAck
      -- | The previous action wasn't an input, so there is no input to echo.
      -- This is: the buffer is empty.
    | ErrEmpty
      -- | There is already a string in the buffer.
    | ErrFull
      -- | Output string.
    | Out String
  deriving (Show, Generic1, Rank2.Foldable, Rank2.Traversable, Rank2.Functor)

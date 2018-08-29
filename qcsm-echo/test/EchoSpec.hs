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
                                                Logic (Bot, Top), Opaque (..),
                                                Reason (Ok), Reference (..),
                                                StateMachine (StateMachine),
                                                Symbolic, checkCommandNames,
                                                forAllCommands,
                                                forAllParallelCommands, genSym,
                                                opaque, prettyCommands,
                                                prettyParallelCommands,
                                                reference, runCommands,
                                                runParallelCommands, (.==))
import           Test.StateMachine.Types       (distribution, generator,
                                                initModel, invariant, mock,
                                                postcondition, precondition,
                                                runner, semantics, shrinker,
                                                spostcondition, transition)
import qualified Test.StateMachine.Types.Rank2 as Rank2

import           Echo                          (Env, input, mkEnv, output)

spec :: Spec
spec = do
    it "implements echo" prop_echoOK
    it "implements echo (parallel)" prop_echoParallelOK


prop_echoOK :: Property
prop_echoOK = forAllCommands echoSM Nothing $ \cmds -> monadicIO $ do
    (hist, _, res) <- runCommands echoSM cmds
    prettyCommands echoSM hist (res === Ok)

prop_echoParallelOK :: Property
prop_echoParallelOK = forAllParallelCommands echoSM $ \cmds -> monadicIO $ do
    prettyParallelCommands cmds =<< runParallelCommands echoSM cmds

echoSM :: StateMachine Model Action IO Response
echoSM = StateMachine
    { initModel = Init
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
      mTransitions Init        MakeEnv    (MakeEnvAck ref) = Empty ref
      mTransitions (Empty ref) (In _ str) InAck            = Buf ref str
      mTransitions (Buf ref _) Echo{}     (Out _)          = Empty ref
      -- TODO: qcsm will match the case below. However we don't expect this to happen!
      -- mTransitions (Buf str) (In _)   _ = Buf str -- Dummy response
          -- error "This shouldn't happen: input transition with full buffer"

      -- | There are no preconditions for this model.
      mPreconditions :: Model Symbolic -> Action Symbolic -> Logic
      mPreconditions Init    MakeEnv = Top
      mPreconditions Init    _       = Bot
      mPreconditions _       MakeEnv = Bot
      mPreconditions Empty{} In{}    = Top
      mPreconditions Buf{}   In{}    = Bot
      mPreconditions Empty{} Echo{}  = Bot
      mPreconditions Buf{}   Echo{}  = Top

      -- | Post conditions for the system.
      mPostconditions :: Model Concrete -> Action Concrete -> Response Concrete -> Logic
      mPostconditions _         MakeEnv MakeEnvAck{} = Top
      mPostconditions Empty{}   In{}   InAck         = Top
      mPostconditions (Buf _ _) In{}   ErrFull       = Top
      mPostconditions _         In{}   _             = Bot
      mPostconditions Empty{}   Echo{} ErrEmpty      = Top
      mPostconditions Empty{}   Echo{} _             = Bot
      mPostconditions (Buf _ str) Echo{}  (Out out)  = str .== out
      mPostconditions (Buf _ _) Echo{}   _           = Bot

      -- | Generator for symbolic actions.
      mGenerator :: Model Symbolic -> Gen (Action Symbolic)
      mGenerator Init        = return MakeEnv
      mGenerator (Empty ref) = In <$> pure ref <*> arbitrary
      mGenerator (Buf ref _) = pure (Echo ref)

      -- | Trivial shrinker.
      mShrinker :: Action Symbolic -> [Action Symbolic]
      mShrinker _ = []

      -- | Here we'd do the dispatch to the actual SUT.
      mSemantics :: Action Concrete -> IO (Response Concrete)
      mSemantics MakeEnv = MakeEnvAck . reference . Opaque <$> mkEnv
      mSemantics (In env str) = do
          success <- input (opaque env) str
          return $ if success
                   then InAck
                   else ErrFull
      mSemantics (Echo env) = maybe ErrEmpty Out <$> output (opaque env)

      -- | What is the mock for?

      -- SA: mock a hack that I hope to remove as soon. It makes it possible to
      -- have a response that returns two references. It was also part of my
      -- modelcheck experiment, removed a couple of commits ago...
      mMock :: Model Symbolic -> Action Symbolic -> GenSym (Response Symbolic)
      mMock _ MakeEnv = MakeEnvAck <$> genSym
      mMock _ In{}    = return InAck
      mMock _ Echo{}  = return (Out "apa")

deriving instance ToExpr (Model Concrete)

-- | The model contains the last string that was communicated in an input
-- action.
data Model (r :: * -> *)
    = Init
    | -- | The model hasn't been initialized.
      Empty (Reference (Opaque Env) r)
    | -- | Last input string (a buffer with size one).
      Buf (Reference (Opaque Env) r)String
  deriving (Eq, Show, Generic)

-- | Actions supported by the system.
data Action (r :: * -> *)
    = MakeEnv
    | -- | Input a string, which should be echoed later.
      In (Reference (Opaque Env) r)String
      -- | Request a string output.
    | Echo (Reference (Opaque Env) r)
  deriving (Show, Generic1, Rank2.Foldable, Rank2.Traversable, Rank2.Functor)

-- | The system gives a single type of output response, containing a string
-- with the input previously received.
data Response (r :: * -> *)
    = MakeEnvAck (Reference (Opaque Env) r)
    | -- | Input acknowledgment.
      InAck
      -- | The previous action wasn't an input, so there is no input to echo.
      -- This is: the buffer is empty.
    | ErrEmpty
      -- | There is already a string in the buffer.
    | ErrFull
      -- | Output string.
    | Out String
  deriving (Show, Generic1, Rank2.Foldable, Rank2.Traversable, Rank2.Functor)

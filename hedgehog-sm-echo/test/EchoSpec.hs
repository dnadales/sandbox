{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Hedgehog               (Callback (Ensure, Update),
                                         Command (Command), Concrete,
                                         Group (Group), HTraversable, MonadGen,
                                         Property, Test, Var, checkParallel,
                                         executeParallel, executeSequential,
                                         forAll, htraverse, property, success,
                                         test, (===))
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import           Echo                   (Echo, input, output, reset)
import           EchoMVar               (mkEchoMVar)
import           EchoTVar               (mkEchoTVar)
import           EchoTVarWrong          (mkEchoTVarWrong)

data State (v :: * -> *) = Empty | Buf String
    deriving (Eq, Ord, Show)

initialState :: State v
initialState = Empty

-- | Input action, containing an input string.
newtype In (v :: * -> *) = In String deriving (Eq, Show)

instance HTraversable In where
    htraverse _ (In str) = pure (In str)

cInput :: forall m n e . (MonadGen n, MonadIO m, Echo e)
       => e -> Command n m State
cInput env =
    Command gen
            execute
            [ Update update
            , Ensure post
            ]
    where
      gen :: State v -> Maybe (n (In v))
      gen Empty   = Just $ fmap In (Gen.element ["foo", "bar", "baz", "buzz"])
      gen (Buf _) = Nothing

      execute :: In v -> m Bool
      execute (In str) = liftIO $ input env str

      update :: State v -> In v -> Var Bool v -> State v
      update st@(Buf _) (In _) _ = st
      -- ^ If there is something in the buffer we expect that the input string
      -- is discarded by the SUT, hence we keep the state as is.
      update Empty (In str) _    = Buf str
      -- ^ If the buffer is empty we add the input string to the buffer.

      post :: State Concrete -> State Concrete -> In Concrete -> Bool -> Test ()
      post Empty    _      (In _) out = out === True
      post (Buf _)  _      (In _) out = out === False

data Out (v :: * -> *) = Out deriving (Eq, Show)

instance HTraversable Out where
    htraverse _ Out = pure Out

cOutput :: forall m n e . (MonadGen n, MonadIO m, Echo e)
        => e -> Command n m State
cOutput env =
    Command gen
            execute
            [ Update update
            , Ensure post
            ]
    where
      gen :: State v -> Maybe (n (Out v))
      gen Empty   = Nothing
      gen (Buf _) = Just $ pure Out

      execute :: Out v -> m (Maybe String)
      execute Out = liftIO $ output env

      update :: State v -> Out v -> Var (Maybe String) v -> State v
      update _ _ _ = Empty
      -- ^ No matter what the initial state is, the 'Empty' action always
      -- leaves the buffer empty.

      post :: State Concrete -> State Concrete -> Out Concrete -> Maybe String -> Test ()
      post Empty _ _ _          = success
      -- ^ The out command can be called in parallel.
      post (Buf str) _ _ outStr = Just str === outStr

prop_echo :: Echo e => IO e -> Property
prop_echo mkEcho = property $ do
    env <- liftIO mkEcho
    actions <- forAll $
        Gen.sequential (Range.linear 1 100)
                       initialState
                       [ cInput env
                       , cOutput env
                       ]
    liftIO $ reset env
    executeSequential initialState actions

prop_echo_par :: Echo e => IO e -> Property
prop_echo_par mkEcho = property $ do
    env <- liftIO mkEcho
    actions <- forAll $
        Gen.parallel (Range.linear 1 100)
                     (Range.linear 1 12)
                     initialState
                     [ cInput env
                     , cOutput env
                     ]
    liftIO $ reset env
    test $ executeParallel initialState actions

main :: IO Bool
main = checkParallel $ Group "EchoSpec"
    [ ("prop_echoTVar", prop_echo mkEchoTVar)
    , ("prop_echoTVarPar", prop_echo_par mkEchoTVar)
    , ("prop_echoMVar", prop_echo mkEchoMVar)
    , ("prop_echoMVarPar", prop_echo_par mkEchoMVar)
    , ("prop_echoTVarWrong", prop_echo mkEchoTVarWrong)
    , ("prop_echoTVarParWrong", prop_echo_par mkEchoTVarWrong)
    ]

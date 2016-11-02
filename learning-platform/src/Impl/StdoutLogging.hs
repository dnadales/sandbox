{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | An implementation of logging using the standard output.
module Impl.StdoutLogging where

import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text               as T
import           DSL.LearningPlatformMTL

newtype StdoutLoggingI m a =
  StdoutLoggingI {runStdoutLogging :: m a}
  deriving (Monad, Functor, Applicative, MonadIO)

instance MonadIO m => LogDSL (StdoutLoggingI m) where
  log level msg = liftIO $ putStrLn $ show level <> ": " <> T.unpack msg


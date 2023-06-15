module Main where

import           Control.Exception
import           System.IO
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

main :: IO ()
main = toplevelExceptionHandler $ do
    forever $ do
      threadDelay 1000000
      putStrLn "Hello, Haskell!"

data MyException = MyException deriving Show

instance Exception MyException

toplevelExceptionHandler :: IO a -> IO a
toplevelExceptionHandler prog = do
    -- Use line buffering in case we have to print big error messages, because
    -- by default stderr to a terminal device is NoBuffering which is slow.
    hSetBuffering stderr LineBuffering
    catches prog [
          Handler rethrowAsyncExceptions
        , Handler rethrowException
      ]
  where
    rethrowAsyncExceptions :: SomeAsyncException -> IO a
    rethrowAsyncExceptions full@(SomeAsyncException e) = do
      putStrLn "Caught an async exception"
      throwIO full

    rethrowException :: SomeException -> IO a
    rethrowException e = do
      putStrLn "Caught some exception"
      throwIO e

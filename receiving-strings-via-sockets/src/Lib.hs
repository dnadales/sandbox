{-# LANGUAGE NamedFieldPuns #-}
module Lib where

import Network.TextViaSockets
import Control.Exception.Base
import Control.Exception

someFunc :: IO ()
someFunc = readLines `catch` handler
    where readLines = do
              conn <- connectTo "localhost" "9090"
              line <- readLineFrom conn
              print line
              line <- readLineFrom conn
              print line
              line <- readLineFrom conn
              print line
              line <- readLineFrom conn
              print line
              close conn
              print "Bye bye!"
          handler :: BlockedIndefinitelyOnSTM -> IO ()              
          handler ex = putStrLn $ "I'm swallowing this: " ++ show ex

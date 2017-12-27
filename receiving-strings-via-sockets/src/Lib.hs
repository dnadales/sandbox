{-# LANGUAGE NamedFieldPuns #-}
module Lib where

import Network.TextViaSockets

someFunc :: IO ()
someFunc = do
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

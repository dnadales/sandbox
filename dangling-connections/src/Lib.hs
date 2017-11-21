{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where


import           Data.String.Utils
import           Network
import           System.IO

someFunc :: IO ()
someFunc = withSocketsDo $ do
    h <- connectTo "localhost" (PortNumber 9090)
    handleCmds h

handleCmds :: Handle -> IO ()
handleCmds h = do
    line <- strip <$> hGetLine h
    case line of
        "quit" -> do
            putStrLn "Closing the handle"
            hClose h
            putStrLn "Bye bye"
        _ -> do
            putStrLn $ "echo \"" ++ line ++ "\""
            hPutStrLn h (reverse line)
            handleCmds h

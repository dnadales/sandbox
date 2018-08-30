-- | WRONG Echo implementation using mutable variables. Used to see whether the
-- state-machine specification would detect the error.

module EchoMVar (mkEchoMVar) where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, swapMVar,
                                          takeMVar, tryPutMVar, tryTakeMVar)
import           Control.Monad           (void)
import           Data.Maybe              (fromMaybe)

import           Echo                    (Echo, input, output, reset)

newtype EchoMVar = EchoMVar
    { _buf :: MVar (Maybe String) }
  deriving Eq

mkEchoMVar :: IO EchoMVar
mkEchoMVar = EchoMVar <$> newMVar Nothing

instance Echo EchoMVar where
    input (EchoMVar mBuf) str = do
        res <- takeMVar mBuf
        case res of
            Nothing -> putMVar mBuf (Just str) >> return True
            Just _  -> putMVar mBuf res >> return False

    output (EchoMVar mBuf) = do
        res <- takeMVar mBuf
        putMVar mBuf Nothing
        return res

    reset (EchoMVar mBuf) = void $ swapMVar mBuf Nothing

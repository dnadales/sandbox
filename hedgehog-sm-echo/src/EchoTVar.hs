-- | Echo implementation using transactional variables.

module EchoTVar (mkEchoTVar) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar,
                                              writeTVar)

import           Echo                        (Echo, input, output, reset)

newtype EchoTVar = EchoTVar
    { _buf :: TVar (Maybe String) }
  deriving Eq

-- | Create a new EchoTVar.
mkEchoTVar :: IO EchoTVar
mkEchoTVar = EchoTVar <$> newTVarIO Nothing

instance Echo EchoTVar where
    -- | Input a string. Returns 'True' iff the buffer was empty and the given
    -- string was added to it.
    input (EchoTVar mBuf) str = atomically $ do
        res <- readTVar mBuf
        case res of
            Nothing -> writeTVar mBuf (Just str) >> return True
            Just _  -> return False

    -- | Output the buffer contents.
    output (EchoTVar mBuf) = atomically $ do
        res <- readTVar mBuf
        writeTVar mBuf Nothing
        return res

    -- | Reset the environment
    reset (EchoTVar mBuf) = atomically $ writeTVar mBuf Nothing


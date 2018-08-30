-- | WRONG Echo implementation using transactional variables.

module EchoTVarWrong (mkEchoTVarWrong) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO,
                                              writeTVar)

import           Echo                        (Echo, input, output, reset)

newtype EchoTVar = EchoTVar
    { _buf :: TVar (Maybe String) }
  deriving Eq

-- | Create a new EchoTVar.
mkEchoTVarWrong :: IO EchoTVar
mkEchoTVarWrong = EchoTVar <$> newTVarIO Nothing

instance Echo EchoTVar where
    -- | Input a string. Returns 'True' iff the buffer was empty and the given
    -- string was added to it.
    input (EchoTVar mBuf) str = do
        res <- readTVarIO mBuf
        case res of
            Nothing -> atomically $ writeTVar mBuf (Just str) >> return True
            Just _  -> return False

    -- | Output the buffer contents.
    output (EchoTVar mBuf) =  do
        res <- readTVarIO mBuf
        atomically $ writeTVar mBuf Nothing
        return res

    -- | Reset the environment
    reset (EchoTVar mBuf) = atomically $ writeTVar mBuf Nothing


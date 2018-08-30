-- | Echo API.

module Echo (Env, input, mkEnv, output, reset) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar,
                                              writeTVar)

newtype Env = Env
    { _buf :: TVar (Maybe String) }
  deriving Eq

-- | Create a new environment.
mkEnv :: IO Env
mkEnv = Env <$> newTVarIO Nothing

-- | Reset the environment
reset :: Env -> IO ()
reset (Env mBuf) = atomically $ writeTVar mBuf Nothing

-- | Input a string. Returns 'True' iff the buffer was empty and the given
-- string was added to it.
input :: Env -> String -> IO Bool
input (Env mBuf) str = atomically $ do
    res <- readTVar mBuf
    case res of
        Nothing -> writeTVar mBuf (Just str) >> return True
        Just _  -> return False

-- | Output the buffer contents.
output :: Env -> IO (Maybe String)
output (Env mBuf) = atomically $ do
    res <- readTVar mBuf
    writeTVar mBuf Nothing
    return res

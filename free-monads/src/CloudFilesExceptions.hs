{-# LANGUAGE DeriveFunctor #-}
-- | How do we deal with fatal errors at the IO layer?
--

module CloudFilesExceptions where

import           Control.Monad.Free

-- | Consider the clould files DSL again, assume we want to deal with two kinds
-- of errors:
--
--   - A file that we want to list does not exist
--   - The connection with the server is refused
--
-- The first kind of errors seem to be specific to the domain logic, so it
-- could be encoded in the DSL. The second kind of error seems to be fatal and
-- dependent on the implementation (we might not be using the network at all in
-- our interpreter).
--
-- So the question is how do we encode the error handling logic, and how do we
-- deal with the fatal errors?
--
-- The fact that non-fatal errors can arise as the result of a command can be
-- modeled by changing the return type in the command that can originate
-- errors. Assume that @ListFiles@ can fail. Then we have the following
-- functor:
data CloudFilesF a = SaveFile Path Bytes a
                   | ListFiles Path (Result -> a)
                   deriving Functor

type Path = String
type Bytes = String
type Result = Either String [Path]

type ClouldFilesM = Free CloudFilesF

saveFile :: Path -> Bytes -> ClouldFilesM ()
saveFile path bytes = liftF $ SaveFile path bytes ()
--  Free (SaveFile path bytes (Pure ()))

listFiles :: Path -> ClouldFilesM  Result
listFiles path = Free (ListFiles path Pure)

-- | Some sample program
prog0 :: ClouldFilesM [Path]
prog0 = do
  saveFile "/myfolder/pepino" "verde"
  saveFile "/myfolder/tomate" "rojo"
  res <- listFiles "/myfolder"
  case res of
    Left err -> return []
    Right xs -> return xs

-- | A simple interpreter.
interp :: CloudFilesF a -> IO a
interp (SaveFile path bytes next) = do
  -- What if a fatal exception would occur here?
  putStrLn $ "Saving " ++ bytes ++ " to " ++ path
  return next



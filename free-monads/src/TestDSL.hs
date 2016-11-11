{-# LANGUAGE DeriveFunctor #-}
-- |

module TestDSL where

import           Control.Monad.Free

data EntityType = Project | Site

data TestCommand e a = Create EntityType (e -> a) | Select e a
  deriving Functor

-- | The DSL
create :: EntityType -> Free (TestCommand e) e
create et = liftF $ Create et id

select :: e -> Free (TestCommand e) ()
select e = liftF $ Select e ()

createN :: Int -> EntityType -> Free (TestCommand e) [e]
createN n et = mapM (const $ create et) [0..n-1]

-- | A sample program:
test :: Free (TestCommand e) ()
test = do
  p <- create Project
  ps <- createN 10 Project
  select p
  _ <- create Site
  return ()

-- | A trivial interpreter.
interpTestCommand :: TestCommand String a -> IO a
interpTestCommand (Create Project withEntity) = do
  putStrLn $ "Creating a project"
  return (withEntity "Project X")
interpTestCommand (Create Site withEntity) = do
  putStrLn $ "Creating a site"
  return (withEntity "Site 51")
interpTestCommand (Select e next) = do
  putStrLn $ "Selecting " ++ e
  return next

-- | Running the interpreter
runTest :: IO ()
runTest = foldFree interpTestCommand test

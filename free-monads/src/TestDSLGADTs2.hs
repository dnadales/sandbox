{-# LANGUAGE GADTs #-}
-- | Could be explicit about the nature of the entities we are manipulating in
-- our DSL while abstracting over implementation details such as concurrency
-- and error handling?
--
-- Based on the discussion at:
--
--    http://stackoverflow.com/a/40496349/2289983
--

module TestDSLGADTs2 where

import           Control.Monad.Free

data Site = Site {sName :: String} deriving Show
data Project = Project {pName :: String} deriving Show

data EntityType e where
  SiteTy :: EntityType Site
  ProjectTy :: EntityType Project

data CommandF m r where
  Create :: EntityType e -> (m e -> r) -> CommandF m r
  Select :: EntityType e -> m e -> r -> CommandF m r

instance Functor (CommandF m) where
  -- fmap :: (a -> b) -> CommandF a -> CommandF b
  fmap f (Create t next) = Create t (f . next)
  fmap f (Select t e next) = Select t e (f next)

type Command m = Free (CommandF m)

create :: EntityType e -> Command m (m e)
create et = Free (Create et Pure)

select :: EntityType e -> m e -> Command m ()
select et me = Free (Select et me (Pure ()) )

-- | A sample program.
test :: Command m ()
test = do
  mp <- create ProjectTy -- p :: m Project
  select ProjectTy mp
  -- Note that calling @select SiteTy mp@ will give a compile time error.
  ms <- create SiteTy
  return ()

-- | An IO interpreter.
interp :: CommandF IO r -> IO r
interp (Create et next) = do
  return $ next (entityTypeToIOEntity et)

interp (Select et em next) = do
  e <- em
  putStrLn $ "selected  " ++ asString et e
  return next

entityTypeToIOEntity :: EntityType e -> IO e
entityTypeToIOEntity SiteTy= return (Site "a site")
entityTypeToIOEntity ProjectTy = return (Project "a project")
                                 -- Note that @return Site@ will result in a
                                 -- type error!

asString :: EntityType e -> e -> String
asString SiteTy _ = "Site"
asString ProjectTy _ = "Project"

-- | Running the interpreter
runTest :: IO ()
runTest = foldFree interp test

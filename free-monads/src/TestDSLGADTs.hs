{-# LANGUAGE GADTs #-}
-- | Test DSL based on the solution by Benjamin Hodgson at SO:
--
--    http://stackoverflow.com/a/40496349/2289983

module TestDSLGADTs where

import           Control.Monad.Free

data Site = Site deriving Show
data Project = Project deriving Show

data EntityType e where
  SiteTy :: EntityType Site
  ProjectTy :: EntityType Project

data CommandF r where
  Create :: EntityType e -> (e -> r) -> CommandF r
  Select :: EntityType e -> e -> r -> CommandF r

instance Functor CommandF where
  -- fmap :: (a -> b) -> CommandF a -> CommandF b
  fmap f (Create t next) = Create t (f . next)
  fmap f (Select t e next) = Select t e (f next)

type Command = Free CommandF

create :: EntityType e -> Command e
create et = liftF $ Create et id
-- | The above should be equivalent to:
--
-- > create et = Free (Create et Pure)
--
-- But why?
--
-- > Free :: f (Free f a) -> Free f a
-- > => {return type of create}
-- > Free :: CommandF (Free CommandF e) -> Free CommandF e
-- > => {application of Free}
-- > Create et Pure :: CommandF (Free CommandF e)
-- > => { Create :: EntityType e -> (e -> r) -> CommandF r
--      , Pure :: a -> Free f a}
-- > et :: EntityType e
-- > Pure :: e -> Free CommandF e
--

select :: EntityType e -> e -> Command ()
select et e = Free (Select et e (Pure ()))

-- | A sample program
test :: Command ()
test = do
  p <- create ProjectTy -- p :: Project!
  select ProjectTy p
  s <- create SiteTy -- s :: Site!
  return ()

entityTypeToIOEntity :: EntityType e -> IO e
entityTypeToIOEntity SiteTy= return Site
entityTypeToIOEntity ProjectTy = return Project
                                 -- Note that @return Site@ will result in a
                                 -- type error!

asString :: EntityType e -> e -> String
asString SiteTy _ = "Site"
asString ProjectTy _ = "Project"

-- | And an interpreter.
interp :: CommandF r -> IO r
interp (Create et next) = do
  e <- entityTypeToIOEntity et
  putStrLn $ "got a " ++ asString et e
  return (next e)
interp (Select et e next) = do
  putStrLn $ "selected  " ++ asString et e
  return next

-- | Running the interpreter
runTest :: IO ()
runTest = foldFree interp test

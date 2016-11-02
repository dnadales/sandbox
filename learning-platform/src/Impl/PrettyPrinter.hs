-- | A dummy (and incorrect) implementation of the LearningPlatformDSL by
-- printing the commands.

module Impl.PrettyPrinter where

import           Control.Monad.IO.Class
import           Data.Monoid
import           DSL.LearningPlatformMTL

data PrettyPrinterI m a =
  PrettyPrinterI {runPrettyPrinter :: m a}


-- | These instances are defined for learning purposes. You could use
-- @deriving@ instead.
instance Functor m => Functor (PrettyPrinterI m) where
  fmap f pp = PrettyPrinterI $ fmap f (runPrettyPrinter pp)

instance Applicative m => Applicative (PrettyPrinterI m) where
  pure x = PrettyPrinterI (pure x)
  ppf <*> ppa = PrettyPrinterI $ (runPrettyPrinter ppf) <*> (runPrettyPrinter ppa)

instance Monad m => Monad (PrettyPrinterI m) where
  ppa >>= ppfppb =  PrettyPrinterI $
                    (runPrettyPrinter ppa) >>=
                    \a -> runPrettyPrinter (ppfppb a)

instance MonadIO m => MonadIO (PrettyPrinterI m) where
  liftIO io = PrettyPrinterI (liftIO io)

-- Now you see why automatic derivation is useful?

-- | We would like to use `putStrLn`. However we cannot simply use:
--
-- @
--     instance LearningPlatformDSL (PrettyPrinterI IO) where
-- @
--
-- since we need a type parameter in the instance declaration. Besides, we
-- would be placing unnecessary constraints.
instance MonadIO m => LearningPlatformDSL (PrettyPrinterI m) where
  -- Notice that the following is boilerplate, and you could make a
  -- PrettyPrinterI an instance of `MonadIO`.
  -- addSkill skill = PrettyPrinterI $ liftIO $ putStrLn "addSkill"
  addSkill = add
  addExercise exercise skill = add (exercise, skill)
  addUser = add
  addDependency skill dependsOn = add (skill, dependsOn)
  complete exercise user = add (exercise, user)
  listSkills user = add ("List skills for", user) >> return []

add elem = liftIO $ putStrLn msg
    where msg = "add" <> show elem


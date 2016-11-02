{-# LANGUAGE OverloadedStrings #-}
-- | A learning platform mockup.

module DSL.LearningPlatformMTL where

import           Data.Foldable
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Prelude       hiding (log)

-- | Types of the domain.
newtype Skill = Skill Text deriving Show
newtype Exercise = Exercise Text deriving Show
newtype User = User Text deriving Show

-- | The DSL for the learning platform.
class Monad m => LearningPlatformDSL m where
  addSkill :: Skill -> m ()
  -- | Add an exercise. For simplicity we assume that all the exercises in the
  -- system are unique.
  addExercise :: Exercise -> Skill -> m ()
  addUser :: User -> m ()
  -- | @addDependency skill dependsOn@ registers @skill@ as dependent on
  -- @dependsOn@.
  addDependency :: Skill -> Skill -> m ()
  complete :: Exercise -> User -> m ()
  -- | Complete multiple exercises
  completeMultiple :: [Exercise] -> User -> m()
  completeMultiple xs u = traverse_ (`complete` u) xs
  listSkills :: User -> m [Skill]


-- | A DSL for logging.
class Monad m => LogDSL m where
  log :: Level -> Text -> m ()

data Level = Debug | Info deriving Show

-- * Some sample scenarios (programs built using the DSL)


-- * Running the scenarios.

-- to run the scenarios we need some implementations for our
-- `LearningPlatformDSL` and `LogDSL`.

-- | Pretty printer for `LearningPlatformDSL`.

-- | Implementation of log using the standard output.

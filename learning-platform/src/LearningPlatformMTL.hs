{-# LANGUAGE OverloadedStrings #-}
-- | A learning platform mockup.

module LearningPlatformMTL where

import           Data.Foldable
import           Data.Monoid
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

data Level = Debug | Info

-- * Some sample scenarios (programs built using the DSL)

s0 = Skill "Algebra"
s1 = Skill "Logic"
s2 = Skill "Functional Programming"
skills = [s0, s1, s2]

e0 = Exercise "Algebra: Exercise 0"
e1 = Exercise "Algebra: Exercise 1"
e2 = Exercise "Logic: Exercise 0"
e3 = Exercise "FP: Exercise 0"
e4 = Exercise "FP: Exercise 1"
exercises = [e0, e1, e2, e3, e4]

u0 = User "Alice"
u1 = User "Bob"
users = [u0, u1]

addSkills :: (LearningPlatformDSL m) => m ()
addSkills = do
  traverse_ addSkill skills
  addDependency s2 s0
  addDependency s2 s1

addExercises :: (LearningPlatformDSL m) => m ()
addExercises = do
  addExercise e0 s0; addExercise e1 s0
  addExercise e2 s1;
  addExercise e3 s2; addExercise e4 s2;

addUsers :: (LearningPlatformDSL m) => m ()
addUsers = do
  traverse_ addUser users
  -- And complete some exercises...
  completeMultiple [e0, e2] u0
  completeMultiple [e0, e1, e2] u1

listSkillsForAllUsers :: (LearningPlatformDSL m, LogDSL m) => m ()
listSkillsForAllUsers = do
  xs <- listSkills u0
  ys <- listSkills u1
  log Debug $ (T.pack (show u0)) <> " has skills " <> (T.pack (show xs))
  log Debug $ (T.pack (show u1)) <> " has skills " <> (T.pack (show ys))

scenario :: (LearningPlatformDSL m, LogDSL m) => m ()
scenario = do
  addSkills
  addExercises
  addUsers
  listSkillsForAllUsers

-- * Running the scenarios.

-- to run the scenarios we need some implementations for our
-- `LearningPlatformDSL` and `LogDSL`.

-- | Pretty printer for `LearningPlatformDSL`.

-- | Implementation of log using the standard output.

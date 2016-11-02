{-# LANGUAGE OverloadedStrings #-}
-- | A sample scenario.

module Examples.Scenario0 where

import           Data.Foldable
import           Data.Monoid
import qualified Data.Text               as T
import           DSL.LearningPlatformMTL
import           Impl.PrettyPrinter
import           Impl.StdoutLogging
import           Prelude                 hiding (log)

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

-- * Running the scenarios

-- | For running the scenarios you'd need a specific instance of the
-- LearningPlatformDSL. For instance:
--
-- > runPrettyPrinter (addUsers :: PrettyPrinterI IO ())
--
-- However, we cannot only use @runPrettyPrinter@ with @scenario@ since it does
-- not provide a @LogDSL@ instance. We could use @runStdoutLogging@.
--
-- Which one is correct?
--
-- @
--   runStdoutLogging $ runPrettyPrinter scenario :: IO ()
--
--   runPrettyPrinter $ runStdoutLogging scenario :: IO ()
-- @
--
-- Well this depends what do you consider to be logical. In the first case,
-- @PrettyPrinterI@ has to provide an instance for @LogDSL@, provided that its
-- inner monad is (which is no problem). In the second case, the
-- @StdoutLoggingI@ has to provide an instance of for @LearningPlatformDSL@
runScenario :: IO ()
runScenario = runStdoutLogging $ runPrettyPrinter scenario

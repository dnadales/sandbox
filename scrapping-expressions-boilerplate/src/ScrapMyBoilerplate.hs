{-# LANGUAGE DeriveDataTypeable #-}
module ScrapMyBoilerplate where

import           Control.Lens.Plated
import           Control.Lens.Setter   (over)
import           Data.Data
import           Data.Data.Lens        (uniplate)
import           Data.Functor.Identity
import           Data.Generics.Aliases
import           Data.Generics.Schemes

newtype Salary = Salary Double deriving (Show, Data, Typeable)

raiseSalary :: (Typeable a) => a -> a
raiseSalary = mkT $ \(Salary s) -> Salary (s * 1.04)

-- Performance of the department.
data Performance = Good | Bad deriving (Show, Data)

type Name = String

type Id = Int

data CompanyAsset = Employee Name Salary
                  | Plant Name
                  | Boss Name Performance Salary [CompanyAsset]
                  | Pet Name
                  | Car Id
                  -- ... and imagine 100 more options
                  deriving (Show, Data)

emp0 = Employee "Laura" (Salary 10)
emp1 = Employee "Pedro" (Salary 8)
emp2 = Employee "Denice" (Salary 20)
emp3 = Employee "Nahuel" (Salary 16)

boss0 = Boss "Nico" Good (Salary 22) [emp0, emp1, boss1]
boss1 = Boss "Karel" Bad (Salary 30) [emp2, emp3]

raiseSalaries :: (Data x) => x -> x
raiseSalaries = everywhere' raiseSalary

-- | What if we want to raise the salaries of good employees only?
raiseEmployeeSalary :: CompanyAsset -> CompanyAsset
raiseEmployeeSalary (Boss n Good s as) =
    Boss n Good (raiseSalary s) (raiseSalaries2 as)
raiseEmployeeSalary (Boss n Bad s as) =
    Boss n Bad s (raiseSalaries2 as)
raiseEmployeeSalary x =
    gmapT raiseSalaries2 x

raiseSalaries2 :: (Data x) => x -> x
raiseSalaries2 = everywhere' (raiseSalary  `extQ` mkT raiseEmployeeSalary)

-- This does not seem to work: the salaries of the Good employees is not raised.
--
-- > λ> raiseSalaries2 boss0
-- > Boss "Nico" Good (Salary 22.880000000000003) [Employee "Laura" (Salary 10.0),Employee "Pedro" (Salary 8.0),Boss "Karel" Bad (Salary 30.0) [Employee "Denice" (Salary 20.0),Employee "Nahuel" (Salary 16.0)]]
--
-- 'gmapT' does not work as expected:
--
-- > λ> gmapT raiseSalary (emp0, emp1)
-- > (Employee "Laura" (Salary 10.0),Employee "Pedro" (Salary 8.0))
--

-- Using the solution suggested by this answer on stack overflow:
--
-- > https://stackoverflow.com/a/47339350/2289983
--
--
-- We should be able to use a traverse that can be automatically derive, the author of the answer is defining this for illustration purposes
traverseCA :: Applicative f => (CompanyAsset -> f CompanyAsset) -> CompanyAsset -> f CompanyAsset
traverseCA f (Boss n p s as) = Boss n p s <$> traverse f as
traverseCA _ x               = pure x

raiseSalaries3 :: CompanyAsset -> CompanyAsset
raiseSalaries3 (Boss n Good s as) = Boss n Good (raiseSalary s) (raiseSalaries3 <$> as)
raiseSalaries3 b@(Boss _ Bad _ _) = b -- No raise for anything that is under this boss.
raiseSalaries3 (Employee n s) = Employee n (raiseSalary s)
raiseSalaries3 x =
    over traverseCA raiseSalaries3 x

-- And this is what you'd get if you would use `Control.Lens.Plated.plate` from `lens`.
instance Plated CompanyAsset where
  plate = uniplate


raiseSalaries4 :: CompanyAsset -> CompanyAsset
raiseSalaries4 (Boss n Good s as) = Boss n Good (raiseSalary s) (raiseSalaries4 <$> as)
raiseSalaries4 b@(Boss _ Bad _ _) = b -- No raise for anything that is under this boss.
raiseSalaries4 (Employee n s) = Employee n (raiseSalary s)
raiseSalaries4 x = -- runIdentity $ plate (pure . raiseSalaries4) x
    over plate raiseSalaries4 x


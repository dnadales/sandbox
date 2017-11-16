{-# LANGUAGE DeriveDataTypeable #-}
module ScrapMyBoilerplate where

import           Data.Data
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


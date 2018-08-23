-- | Playing with fix points.

module PwFix where

import           Data.Function (fix)

-- | Factorial function, open recursion style.
--
-- The last parenthesis are not needed. They are put there just for the sake of
-- clarity.
faco :: (Int -> Int) -> (Int -> Int)
faco _ 1 = 1
faco r n = n * r (n - 1)

-- How do we make 'fac' to call itself?, remember fix!
--
-- > fix :: (a -> a) -> a
--
-- Making a ~ Int -> Int gives
--
-- > fix :: ((Int -> Int) -> (Int -> Int)) -> (Int -> Int)
--
-- Then our factorial function becomes:
--
fac :: Int -> Int
fac = fix faco

-- What just happened?
--
-- > fix faco 5
-- > = { def. 'fix' }
-- > faco (fix faco) 5
-- > = { def. 'faco'}
-- > 5 * (fix faco) 4
-- > ...
--
-- I hope you see what's happening now, and that you can prove by induction
-- that @fix faco = facr@, where @facr@ is the factorial function defined using
-- recursion.
--

skimo :: (Int -> Int) -> Int -> Int
skimo _ 1 = 1
skimo r n = faco r n - 1

skim :: Int -> Int
skim = fix skimo

-- | OO in Haskell.
--
-- Taken from http://www.well-typed.com/blog/2018/03/oop-in-haskell/

module PwFix.Inheritance where

import           Data.Function (fix)

data Counter = Counter
    { tick    :: Counter
    , display :: Int
    }

mkCounter :: (Int -> Counter) -> Int -> Counter
mkCounter self n = Counter
    { tick = self (n + 1)
    , display = n
    }

zero = fix mkCounter 0
three = tick (tick (tick zero))

-- Note that if Counter would be an instance of show and you'd try to show
-- 'zero' the recursion will never end!

mkDoubleCounter :: (Int -> Counter) -> Int -> Counter
mkDoubleCounter self n = (mkCounter self n)
--    { display = n * 2}
   { tick = self (n + 2) } -- Note that this will work as well, and in this
                           -- case we're ticking twice each time 'tick' is called.

zeroD = fix mkDoubleCounter 0
six = tick (tick (tick zeroD))

{-# LANGUAGE RankNTypes #-}
-- | Extending the state of a counter.

module PwFix.ExtendingState where

import           Control.Lens        (Lens')
import           Control.Lens.Getter (view)
import           Control.Lens.Setter (over)
import           Control.Lens.Tuple  (_1, _2)
import           Data.Function       (fix)

data Counter = Counter
    { tick    :: Counter
    , tock    :: Counter
    , display :: String
    }

mkCounter :: Lens' st Int
          -- ^ Lens to zoom in the integer value of the state.
          -> (st -> Counter)
          -> (st -> Counter)
mkCounter l self st = Counter
    { tick = self (over l (+1) st)
    , tock = self (over l (+1) st)
    , display = show (view l st)
    }


-- | This won't work!
--
-- >>> display $ fix ticktock (0, 0)
-- "0"
--
-- Not the "(0, 0)" you'd expect!
--
-- ticktock :: ((Int, Int) -> Counter) -> (Int, Int) -> Counter
-- ticktock self (n, m) = mkCounter (self . _reconstruct) n
--     where _reconstruct :: Int -> (Int, Int)
--           _reconstruct i = (i, m)

-- Let's try with lenses!
ticktock :: Lens' st (Int, Int)
         -> (st -> Counter)
         -> (st -> Counter)
ticktock l self st = (mkCounter (l . _1) self st)
    { -- > tick = self (over (l . _1) (+1) st)
      -- Note that the above is not needed since it is equivalent to the tick
      -- function defined at mkCounter.
      tock = self (over (l . _2) (+1) st)
    , display = show (view l st)
    }

esExample = display (tick (tock (tick (fix (ticktock id) (0, 0)))))

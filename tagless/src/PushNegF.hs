{-# LANGUAGE FlexibleInstances #-}
module PushNegF where

import           ExpSYM

-- How to write 'pushNeg' using the final approach? You cannot pattern match!
--
--
-- > pushNeg :: (ExpSYM repr) => repr -> repr
-- > pushNeg = ???
--

-- | The context-dependency is made implicit by using a context.
data Ctx = Pos | Neg

instance ExpSYM repr => ExpSYM (Ctx -> repr) where
    -- Can you define this instance without looking at the solution?
    lit i Pos = lit i
    lit i Neg = neg (lit i)
    neg e Pos = e Neg -- <= this is a tricky one!
    neg e Neg = e Pos
    -- The equation below seems to correspond with the rule:
    -- - (e0 + e1) == -e0 + - e1
    add e0 e1 ctx = add (e0 ctx) (e1 ctx)

-- pushNeg :: _ -- ExpSYM repr => repr -> repr
pushNeg :: (Ctx -> e) -> e
pushNeg e = e Pos

-- Let't try this operation
--
-- > ghci> view tf1
-- > "(8 + - ((1 + 2)))"
-- > it :: String
-- > ghci> view (PushNegF.pushNeg tf1)
-- > "(8 + (- (1) + - (2)))"
-- >  it :: String
-- > ghci> eval tf1
-- > 5
-- > it :: Int
-- > ghci> eval (PushNegF.pushNeg tf1)
-- > 5
-- > it :: Int
--
-- Note how in the expression @PushNegF.pushNeg tf1@ we're building a function
-- from an existing representation!

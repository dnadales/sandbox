{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ExpSYM where

-- | Representation of expressions over a semantic domain @repr@.
class ExpSYM repr where
    lit :: Int -> repr
    neg :: repr -> repr
    add :: repr -> repr -> repr

--------------------------------------------------------------------------------
-- Examples of expressions
--------------------------------------------------------------------------------
tf1 :: ExpSYM e => e
tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))

--------------------------------------------------------------------------------
-- Instances of @ExpSYM@
--------------------------------------------------------------------------------

-- | Choice of 'Int' as the semantic domain.
instance ExpSYM Int where
    lit = id
    neg = negate
    add = (+)

-- | Evaluate the semantic domain of 'Int' to an 'Int'
eval :: Int -> Int
eval = id

-- Now you can evaluate terms:
--
-- > eval tf1
-- > 5
-- > it :: Int
--
-- The type of eval looks silly, but you need it so that the compiler can
-- select the right instance.

-- | Choice of 'String' as the semantic domain.
instance ExpSYM String where
    lit = show
    neg n = "- (" ++ n ++ ")"
    add n0 n1 = "(" ++ n0 ++ " + " ++ n1 ++ ")"

-- | This function can be used to pick the 'String' instance.
view :: String -> String
view = id

--  Now you can print terms, by choosing the string instance!
--
-- > view tf1
-- > "(8 + - ((1 + 2)))"
-- > it :: String
--
-- Of course, you could have just typed:
--
-- > tf1  :: String
-- > "(8 + - ((1 + 2)))"
-- > it :: String
--


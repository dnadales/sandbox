-- | Tagless final embedding with higher-order abstract syntax.
module LambdaCalc.TFEHOAS where

class Symantics repr where
    int :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int

    lam :: (repr a -> repr b) -> repr (a -> b)
    app :: repr (a -> b) -> repr a -> repr b

-- Some terms in this language
th1 :: (Symantics repr) => repr Int
th1 = add (int 1) (int 2)

th2 :: (Symantics repr) => repr (Int -> Int)
th2 = lam (\x -> add x x)

th3 :: (Symantics repr) => repr ((Int -> Int) -> Int)
th3 = lam (\x -> add (app x (int 1)) (int 2))

-- Note that you cannot even express an open term like:
--
-- > lam (add z (s z))
--
-- Since you need to use a lambda abstraction or other function!

-- Some othe interesting examples:
--
-- >>> :t lam (\x -> lam (\y -> x + y))
-- lam (\x -> lam (\y -> x + y))
--   :: (Num (repr b), Symantics
--

--------------------------------------------------------------------------------
-- Interpreters
--------------------------------------------------------------------------------

newtype R a = R { unR :: a }

-- | An evaluator:
instance Symantics R where
    int = R

    add e0 e1 = R $ unR e0 + unR e1

    lam f = R $ unR . f . R -- This is: \a -> unR $ f (R a)

    app f e = R $ (unR f) (unR e)

eval :: R a -> a
eval = unR

newtype S a = S { unS :: Int -> String }
-- | A pretty printer:
instance Symantics S where
    int i = S $ const (show i)

    add e0 e1 = S $ \i -> unS e0 i ++ " + " ++ unS e1 i

    lam f = S $ \i ->
                    let x = "x" ++ show i
                        sx = S $ const x

                    in "\\" ++ x ++ " -> " ++ unS (f sx) (i + 1)

    app f e = S $ \i -> "(" ++ unS f i ++ " " ++ unS e i ++ ")"

-- | Pretty printer:
--
-- >>>  view th1
-- "1 + 2"
--
-- >>> view th2
-- "\\x0 -> x0 + x0"
--
-- >>> view th3
-- "\\x0 -> (x0 1) + 2"
--
view :: S a -> String
view e = unS e 0

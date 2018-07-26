-- | Tagless final embedding with higher-order abstract syntax.
module LambdaCalc.TFEHOAS where

import qualified Data.Function as F

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

--------------------------------------------------------------------------------
-- Extending the interpreter
--------------------------------------------------------------------------------

class MulSYM repr where
    mul :: repr Int -> repr Int -> repr Int

class BoolSYM repr where
    bool :: Bool -> repr Bool
    leq :: repr Int -> repr Int -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a

class FixSYM repr where
    fix :: (repr a -> repr a) -> repr a

-- | Power function defined in this language:
tpow :: (Symantics repr, MulSYM repr, BoolSYM repr, FixSYM repr)
     => repr (Int -> Int -> Int)
tpow = lam (\x -> fix (\self -> lam (\n ->
                                         if_ (leq n (int 0))
                                             (int 1)
                                             (mul x (app self (add n (int (-1)))))
                                         )
                      )
           )

tpow7 :: (Symantics repr, MulSYM repr, BoolSYM repr, FixSYM repr)
      => repr (Int -> Int)
tpow7 = lam (\x -> (tpow `app` x) `app` int 7)

tpow72 :: (Symantics repr, MulSYM repr, BoolSYM repr, FixSYM repr)
       => repr Int
tpow72 = app tpow7 (int 2)

instance MulSYM R where
    mul e0 e1 = R $ unR e0 * unR e1

instance BoolSYM R where
    bool b = R b
    leq e0 e1 = R $ unR e0 <= unR e1
    if_ g e0 e1 = R $ if unR g then unR e0 else unR e1

instance FixSYM R where
    fix fr = R $ F.fix (\a -> unR . fr . R $ a)

--------------------------------------------------------------------------------
-- Extending the pretty printer.
--------------------------------------------------------------------------------

instance MulSYM S where
    mul e0 e1 = S $ \i -> unS e0 i ++ " * " ++ unS e1 i

instance BoolSYM S where
    bool = S . const . show

    leq e0 e1 = S $ \i -> unS e0 i ++ " <= " ++ unS e1 i

    if_ g e0 e1 = S $ \i ->
        "(if " ++ unS g i ++ " then " ++ unS e0 i ++ " else " ++ unS e1 i ++ ")"

instance FixSYM S where
    fix fr = S $ \i ->
        let self = "self" ++ show i in
            "(fix " ++ self ++ " . "++  unS (fr (S $ const self)) (i + 1) ++ ")"


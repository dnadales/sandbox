-- | A higher-order typed language using a typed-tagless final encoding.
module LambdaCalc.HOTLTTF where

class Symantics repr where
    int :: Int -> repr h Int
    add :: repr h Int -> repr h Int -> repr h Int

    z :: repr (a, h) a
    s :: repr h a -> repr (any, h) a

    lam :: repr (a, h) b -> repr h (a -> b)
    app :: repr h (a -> b) -> repr h a -> repr h b

--  We can ask GHCI for the type of the following expressions...
--
-- >>> :t add (int 1) (int 2)
-- add (int 1) (int 2) :: Symantics repr => repr h Int
--

td1 :: (Symantics repr) => repr h Int
td1 = add (int 1) (int 2)

td2o :: Symantics repr => repr (Int, h) (Int -> Int)
td2o = lam (add z (s z))

-- Compare this with:
--
-- >>> :t lam (add z z)
-- lam (add z z) :: Symantics repr => repr h (Int -> Int)
--
-- >>> :t lam (add (s z) (s (s z)))
-- lam (add (s z) (s (s z)))
--  :: Symantics repr => repr (Int, (Int, h)) (a -> Int)
--
-- >>>  lam (int 1)
-- lam (int 1) :: Symantics repr => repr h (a -> Int)
--

-- And you get nice type errors:
--
-- >>> app (int 1) (int 2)
-- <interactive>:439:6: error:
--    • Couldn't match type ‘Int’ with ‘Int -> b’
--      Expected type: repr h (Int -> b)
--        Actual type: repr h

--------------------------------------------------------------------------------
-- Interpreters
--------------------------------------------------------------------------------


newtype R h a = R { unR :: h -> a }

-- | An evaluator for the language.
instance Symantics R where

    int i = R $ const i

    add f0 f1 = R $ \h -> unR f0 h + unR f1 h

    z = R fst

    s v = R $ \(_, h) -> unR v h

    lam e = R $ \h -> \a -> unR e (a, h)

    app f e = R $ \h -> unR f h (unR e h)

eval :: R () a -> a
eval e = unR e ()

-- Let's evaluate our expressions!
--
-- >>> eval td1
-- 3
--
-- >>> eval $ app (lam (add z (int 2))) (int 8)
-- 10
--
-- We can't evaluate open terms, of course:
--
-- >>> eval td2o
-- <interactive>:456:6: error:
--    • Couldn't match type ‘(Int, h0)’ with ‘()’
--       Expected type: R () (Int -> Int)
--       Actual type: R (Int, h0) (Int -> Int)

newtype S h a = S { unS :: Int -> String }

-- | A pretty printer
instance Symantics S where
    int i = S $ const (show i)

    add e0 e1 = S $ \i -> unS e0 i ++ " + " ++ unS e1 i

    z = S $ \i -> "x" ++ show (i - 1)

    s v = S $ \i -> unS v (i - 1)

    -- Note how in a lambda expression we increment the index to keep track of
    -- the level of nesting.
    lam e = S $ \i -> "(\\" ++ "x" ++ show i ++ " -> " ++ unS e (i + 1) ++ ")"

    app f e = S $ \i -> "(" ++ unS f i ++ " " ++ unS e i ++ ")"

-- | Some examples:
--
--
-- >>> view (int 2)
-- "2"
--
-- >>> view td1
-- "1 + 2"
--
-- >>> view $ lam (add z (int 2))
-- "(\\x0 -> x0 + 2)"
--
view :: S () a -> String
view e = unS e 0

{-# LANGUAGE GADTs #-}
-- | A higher-order typed language using GADT's.
module LambdaCalc.HOTL where

-- | An expression with environment @env@ having type @t@.
data Exp env t where
    -- | Boolean literals have the type 'Bool' in any environment.
    B :: Bool -> Exp env Bool

    -- | Declaring a variable requires a value of type 'Var'. Notice how this
    -- function requires that the environment contains a variable of type @t@.
    V :: Var env t -> Exp env t

    -- | Lambda abstractions. A variable of type @a@ is injected into the
    -- environment of the lambda expression (first argonemt of the function).
    L :: Exp (a, env) b -> Exp env (a -> b)

    -- | Function application.
    A :: Exp env (a -> b) -> Exp env a -> Exp env b

data Var env t where
    -- | An environment of the form @(t, env)@ has by definition a variable of
    -- type @t@, which happens to be in the first component fo the tuple.
    VZ :: Var (t, env) t

    -- | If @env@ contains a variable of type @t@, then @(a, env)@ also
    -- contains a variable of type @t@ for any type @a@.
    VS :: Var env t -> Var (a, env) t

-- Some terms of the language:

-- | This should give true @(\x -> x) True@.
--
--
-- > :t ti1
-- > ti1 :: Exp env Bool
--
ti1 = A (L (V VZ)) (B True)

-- This will result in an error
--
-- > ti2a = A (B True) (B False)
-- >
-- >    • Couldn't match type ‘Bool’ with ‘Bool -> b’
-- >      Expected type: Exp env (Bool -> b)
-- >        Actual type: Exp env Bool
-- >    • In the first argument of ‘A’, namely ‘(B True)’
-- >      In the expression: A (B True) (B False)
-- >      In an equation for ‘ti2a’: ti2a = A (B True) (B False)
-- >

-- | Here we're looking up a variable that doen't exist.
--
-- > VZ :: Var (t, env) t
-- > =>
-- > VS VZ :: Var (a, (t, env)) t
-- > =>
-- > V (VS VZ) :: Exp (a, (t, env)) t
-- > =>
-- > L (V (VS VZ)) :: Exp (t, env) (a -> t)
--
-- > B True :: Exp env Bool
--
-- > A (L (V (VS VZ))) (B True) :: Exp (t, env) t
--
ti2o :: Exp (b, env) b
ti2o = A (L (V (VS VZ))) (B True)

-- Compare with the closed term:
--
ti2c :: Exp env Bool
ti2c = A (L (V VZ)) (B True)

-- And what happens with higher indices?
ti2oo :: Exp (a, (b, env)) b
ti2oo = A (L (V (VS (VS VZ)))) (B True)

ti2ooo :: Exp (a1, (a2, (b, env))) b
ti2ooo = A (L (V (VS (VS (VS VZ))))) (B True)

-- | Evaluator for the terms of this language.

eval :: env -> Exp env t -> t
eval _ (B b)       = b
eval env (V v)     = lookp v env
eval env (L e)     = \a -> eval (a, env) e -- We introduce the variable in the environment.
eval env (A e0 e1) = (eval env e0) (eval env e1)


-- | Some examples:
--
-- > lookp VZ ("foo", undefined)
-- > "foo"
-- >
-- > lookp VZ ()
-- >
-- > <interactive>:339:10: error:
-- >    • Couldn't match expected type ‘(t, env0)’ with actual type ‘()’
--
lookp :: Var env t -> env -> t
lookp VZ (t, env)     = t -- Note that this is the only possible type!
lookp (VS v) (a, env) = lookp v env -- Ditto!


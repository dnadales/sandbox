-- | Toy language, as described in http://goo.gl/ZtgEg0

module Toy where

data Toy b next =
  Output b next -- Output b and perform the next aciton.
  | Bell next   -- Rings the bell and perform the next action.
  | Done        -- End of execution.

-- | Try some examples in the REPL:
--
-- > ghci> :t Output 'a' Done
-- > Output 'a' Done :: Toy Char (Toy b next)
--
-- > ghci> :t Bell (Output 'a' Done)
-- > Bell (Output 'a' Done) :: Toy b (Toy Char (Toy b1 next))
--
-- So it seems the types are getting more complex. We can reduce the nexted
-- types by the use of Fix:

data Fix f = Fx (f (Fix f)) -- Fix is for `fixed point of a functor`.

-- If we analyze the kind of fix we see:
--
-- > *Main Toy> :k Fix
-- > Fix :: (* -> *) -> *
--
-- So we see that 'Fix' takes a type-constructor of kind (* -> *).
--
-- Let's check some types:
--
-- > *Main Toy> :t Fix Done
-- > Fix Done :: Fix (Toy b)
--
-- Why?
--
-- Why this does not work:
-- > *Main Toy> :t Fix (Output 'a')
-- >
-- > <interactive>:1:6:
-- >     Couldn't match type ‘Toy Char next’ with ‘Fix ((->) next)’
-- >     Expected type: next -> Fix ((->) next)
-- >       Actual type: next -> Toy Char next
-- >     In the first argument of ‘Fix’, namely ‘(Output 'a')’
-- >     In the expression: Fix (Output 'a')

data Expr = Const Int
  | Add Expr Expr
  | Mul Expr Expr

-- '1 + 2' can be expressed as:
-- > *Toy> :t Const 1 `Add` Const 2
-- > Const 1 `Add` Const 2 :: Expr
--

-- How can we generate Expr from ExprF below?
--
-- Notice that here we are abstracting away the recursive nature of 'Expr':

data ExprF a = ConstF Int
  | AddF a a
  | MulF a a

-- How do we use this to express '1 + 2'?
--
-- > *Toy> :t ConstF 1 `AddF` ConstF 2
-- > ConstF 1 `AddF` ConstF 2 :: ExprF (ExprF a)
--
-- We see how, as with the 'Toy' example above, our types are growing.
--
-- Could we use 'Fix' to reduce this?
--
-- First notice the following:
--
-- > *Toy> :t (ConstF 1)
-- > (ConstF 1) :: ExprF a
--
-- 'ExprF a` has kind '*' (since ExprF has kind '* -> *')
--
-- Why can Fix be applied to somehting that is not a functor then? Because `f`
-- could be any type that takes a parameter. See what happens when we try to
-- apply `Fix` to `Char`:
--
-- > *Toy> Fix 'a'
-- >
-- > <interactive>:31:5:
-- >     Couldn't match expected type ‘f (Fix f)’ with actual type ‘Char’
-- >     Relevant bindings include it :: Fix f (bound at <interactive>:31:1)
-- >     In the first argument of ‘Fix’, namely ‘'a'’
-- >     In the expression: Fx 'a'
-- >     In an equation for ‘it’: it = Fx 'a'
-- >
--
-- Now why does the following type-checks?
--
-- > *Toy> :t Fx (ConstF 1)
-- > Fx (ConstF 1) :: Fix ExprF
--
-- The question is then, how does the compiler infers that the type of 'ConstF
-- 1':
--
-- > ExprF a
--
-- Is equivalent to:
--
-- > f (Fix f)
--
-- What is 'f'? Apparently 'f' is 'ExprF':
--
-- > *Toy> :t ConstF 1 :: ExprF (Fix ExprF)
-- > ConstF 1 :: ExprF (Fix ExprF) :: ExprF (Fix ExprF)
--
-- Well, notice that (ConstF 1) is a type that takes a paramter! So by saying
--
-- > ConstF 1 :: ExprF (Fix ExprF)
--
-- We are instanciating it! We could as well have said:
--
-- *Toy> :t ConstF 1 :: ExprF (String -> Char)
-- ConstF 1 :: ExprF (String -> Char) :: ExprF (String -> Char)
--
-- And it would have typechecked!
--
-- So comming back to our original question, can we write
--
-- > ConstF 1 `AddF` ConstF 2
--
-- And avoid have types growing?
--
-- > *Toy> :t Fix (ConstF 1)
-- > Fix (ConstF 1) :: Fix ExprF
--
-- > *Toy> :t Fix (ConstF 1) `AddF` Fix (ConstF 2)
-- Fix (ConstF 1) `AddF` Fix (ConstF 2) :: ExprF (Fix ExprF)
--
-- > *Toy> :t Fix (Fix (ConstF 1) `AddF` Fix (ConstF 2))
-- > Fix (Fix (ConstF 1) `AddF` Fix (ConstF 2)) :: Fix ExprF








-- For more info see:
-- https://goo.gl/CcfCPX

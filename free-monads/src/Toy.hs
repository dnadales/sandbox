-- | Toy language, as described in http://goo.gl/ZtgEg0

module Toy where

import           Control.Monad.Free

data Toy b next =
  Output b next -- Output b and perform the next aciton.
  | Bell next   -- Rings the bell and perform the next action.
  | Done        -- End of execution.


-- | Let's write an interpreter for this language. We simply convert this to a String.
interpret :: Show a => Fix (Toy a) -> String

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


-- In the same way, we can find a fix-point for the types that our interpreter
-- takes:
--
-- > *Main Toy> :t Fx (Done)
-- > Fx (Done) :: Fix (Toy b)
-- > *Main Toy> :t Fx (Bell (Fx (Done)))
-- > Fx (Bell (Fx (Done))) :: Fix (Toy b)
-- > *Main Toy> :t Fx (Output 'a' (Fx (Bell (Fx (Done)))))
-- > Fx (Output 'a' (Fx (Bell (Fx (Done))))) :: Fix (Toy Char)
--
-- Then Fix can be used to define a type for our interpreter.

-- Ok, having seen how Fix works, let's write the interpreter:
--interpret Fx (FixDone)
interpret (Fx Done) = "done\n"
interpret (Fx (Bell next)) = "ding!\n" ++ interpret next
interpret (Fx (Output a next)) = show a ++ "\n"++ interpret next

-- It works!
--
-- > *Toy> putStr  $ interpret $  Fx (Output 'a' (Fx (Bell (Fx (Done)))))
-- > 'a'
-- > ding!
-- > done
--
-- But what if we want to pass an incomplete program? We cannot interpret this:
--
-- > *Toy> interpret $  Fx (Output 'a' (Fx (Bell )))
-- >
-- > <interactive>:27:34:
-- >     Couldn't match expected type ‘Toy Char (Fix (Toy Char))’
-- >                 with actual type ‘next0 -> Toy b0 next0’
-- >     Probable cause: ‘Bell’ is applied to too few arguments
-- >     In the first argument of ‘Fx’, namely ‘(Bell)’
-- >     In the second argument of ‘Output’, namely ‘(Fx (Bell))’
--
-- So let's define a data type to write incomplete Toy programs, or
-- expressions.

-- (?) Why do we need the `e` here?
data FixE f e = FxE (f (FixE f e)) | Throw e

-- ...

-- Does catch type-checks?
catch :: (Functor f) =>
         FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Throw e) g = g e -- ::? FixE f e2, well yes, look at the type of `e` and `g`.

-- For the second expression we have:
--
-- > x :: f (FixE f e)
-- > flip catch g :: FixE f e1 -> FixE f e2
-- > fmap :: (Functor f) -> (FixE f e1 -> FixE f e2) -> f (FixE f e1) -> f (FixE f e2)
-- > fmap (flip catch g) x :: f (FixE f e2)
-- > FxE (fmap (flip catch g) x) :: FxE f e2
--
catch (FxE x) g = FxE (fmap (flip catch g) x)

-- The we need to define an instance of functor:
instance Functor (Toy b) where
  fmap f (Output x next) = Output x (f next)
  fmap f (Bell     next) = Bell     (f next)
  fmap _  Done           = Done

data Hole = Hole

--  Let's use this to defini incomplete programs:
--
-- > *Toy> :t FxE (Output 'a' (Throw Hole))
-- > FxE (Output 'a' (Throw Hole)) :: FixE (Toy Char) Hole
--
-- > *Toy> :t FxE (Output 'a' (Throw Hole)) `catch` (\_ -> FxE Done)
-- > FxE (Output 'a' (Throw Hole)) `catch` (\_ -> FxE Done)
-- >  :: FixE (Toy Char) e2
--

-- Can FixE f be defined as a functor? The definition of functor I came up with
-- applies function g to the exception value.
--
-- Note that in this case, the parameter of (FixE f) is an exeption!
instance (Functor f) => Functor (FixE f) where
  -- (?) What about the type parameter?
  -- (a -> b) -> (FixE f) a -> (FixE f) b
 fmap g (Throw e) = Throw (g e)
 -- In this definition do not confuse the fmap of FixE with the fmap of functor
 -- g (used at the lambda expression).
 -- fmap g (FxE fs) = FxE (fmap (\fxe -> fmap g fxe) fs)
 -- Note that the above can be written as:
 fmap g (FxE fs) = FxE (fmap (fmap g) fs)

 -- EXERCISE: Does this definition satisfies the functor laws?

instance (Functor f) => Applicative (FixE f) where
  -- The following implementation is easy!
  pure e = Throw e
  -- What about <*>?
  --
  -- (<*>) :: (FixE f) (a -> b) -> (FixE f) a -> (FixE f) b
  --
  -- Note that the first argument of <*> is a FixE value that has a function as
  -- exception type.
  (<*>) (Throw eab) (Throw a) = Throw (eab a)
  -- (<*>) (Throw eab) (FxE fs) = FxE (fmap (\fxe -> (Throw eab) <*> fxe) fs)
  (<*>) (Throw eab) (FxE fs) = FxE (fmap ((Throw eab) <*>) fs)
  -- (<*>) (FxE fs) fxa = FxE (fmap (\fxe -> fxe <*> fxa) fs)
  (<*>) (FxE fs) fxa = FxE (fmap (<*> fxa) fs)

-- How would you define a monad instance of FixE?
instance (Functor f) => Monad (FixE f) where
  return = pure
  -- (>>=) :: (FixE f) a -> (a -> (FixE f) b) -> (FixE f) b
  (Throw a) >>= f = f a
  -- (FxE fs) >>= f = FxE (fmap (\fxe -> fxe >>= f) fs)
  -- Note that the above can be written as:
  (FxE fs) >>= f = FxE (fmap (>>= f) fs)

-- Ok, we defined out instance of monad for 'FixE', but it turns out that we
-- already have an isomorphic type, called... drums.... "The free monad!"

-- To be able to instance this, you require to define applicative and functor for (Free f)
--
-- > instance (Functor f) => Monad (Free f) where
-- >     return = Pure
-- >     (Free x) >>= f = Free (fmap (>>= f) x)
-- >     (Pure r) >>= f = f r
--
-- So we'll just import 'Control.Monad.Free'

-- We want to wrap out Toy constructs into free monads. Doing this requires
-- some boilerplate:
--
-- > output :: a -> Free (Toy a) ()
-- > output x = Free (Output x (Pure ()))
-- >
-- > bell :: Free (Toy a) ()
-- > bell = Free (Bell (Pure ()))
-- >
-- > done :: Free (Toy a) r
-- > done = Free Done
--
-- We can use 'liftF' to scrap this boilerplate:
--
-- > liftF :: (Functor f, MonadFree f m) => f a -> m a
--
-- Here m is 'Free' (our monad) and 'Toy a' is f (our functor).

output :: a -> Free (Toy a) ()
output x = liftF ((Output x) ())

-- Next: we want to get here:
program :: Free (Toy Char) r
program = do
  subroutine
  bell
  done

-- TODO: write a version of the interpreter 'interpret' that uses the free
-- monad.
interpretF :: Show a => Fix (Toy a) -> String

-- TODO: Does 'done' swallows the commands? Why?

-- For more info see:
-- https://goo.gl/CcfCPX

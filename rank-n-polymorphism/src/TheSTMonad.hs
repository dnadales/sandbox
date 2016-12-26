{-# LANGUAGE RankNTypes #-}
-- | Example given in "Lazy Functional State Threads".

module TheSTMonad where

data ST s a

data MutVar s a

newVar :: a -> ST s (MutVar s a)
newVar = undefined

readVar :: MutVar s a -> ST s a
readVar = undefined

writeVar :: MutVar s a -> a -> ST s ()
writeVar = undefined

-- | Let's see what happens when we define 'runST' as follows:
runST :: ST s a -> a
runST = undefined

-- | Function 'evil' allows to read the variable created by another thread!
--
-- We have:
--
-- > newVar True :: ST s (MutVar s Bool)
-- > runST (newVar True) :: MutVar s Bool
-- > v :: MutVar s Bool
-- > readVar v :: ST s Bool
-- > runST (readVar v) :: Bool

evil :: Bool
evil =
  let v = runST (newVar True)
  in
    runST (readVar v)

-- | But what happens if we define 'runST' with a 'forall' quantifier:
--
-- > runST' :: (forall s. ST s a) -> a
--
-- Then in our @evil'@ function, that uses @runST'@, in the expression:
--
-- > runST' (readVar v) :: Bool
--
-- We need
--
-- > readVar v :: (forall s. ST s Bool)
--
-- However we have:
-- > newVar True :: ST s (MutVar s Bool)
-- > runST (newVar True) :: MutVar s Bool
-- > v :: MutVar s Bool
-- > readVar v :: ST s Bool
--
-- I think the upshot here is that @runST'@ has to return the same type @a@ for
-- all the states @s@, and this makes it impossible for @s@ to appear in the
-- result.
--
-- See also: https://en.wikibooks.org/wiki/Haskell/Polymorphism

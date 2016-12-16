{-# LANGUAGE RankNTypes #-}
module ScottVsChurchEncoding where

import           Prelude hiding (map, sum)

data List a = Nil | Cons a (List a)

-- See fixity declarations in Haskell.
infixr 5 `Cons`

-- | Deconstruct a list applying the functions passed as parameters.
uncons :: (a -> List a -> r) -- ^ Function to apply in the 'Cons' case.
       -> r                   -- ^ Result to yield in the 'Nil' case.
       -> List a
       -> r
uncons _ ni Nil = ni
uncons co _ (Cons x xs) = co x xs

-- | Let's use this function to compute the sum of a list.
sum :: List Int -> Int
sum xs = uncons (\x ys -> x + sum ys) 0 xs

-- | What about using `uncons` to decide whether the list is null?
isNull :: List a -> Bool
isNull = uncons (\_ _ -> False) True

-- | And what about defining the 'map' function?
map :: (a -> b) -> List a -> List b
map f = uncons (\x xs -> Cons (f x) (map f xs)) Nil

as = 0 `Cons` 1 `Cons` 2 `Cons` 3 `Cons` Nil
r0 = sum as

-- | "A list is fully determined by what happens" when we 'uncons' it.
--
-- Note that we need to use the 'forall' keyword here, otherwise the compiler
-- will complain that variable 'r' is not defined.
newtype ListS a =
  ListS {unconsS :: forall r. (a -> ListS a -> r) -> r -> r}

-- | How do we define list operations using this representation?
sumS :: ListS Int -> Int
-- Where do you get the list from?
sumS xss = (unconsS xss) (\x ys -> x + sumS ys) 0

-- | How do we define a list to which we can apply 'sumS'. More generally: how
-- do we construct lists of type 'ListS'.
--
-- We need to define functions analogous to 'Nil' and 'Cons'!
nilS :: ListS a
nilS = ListS (\_ ni -> ni)

consS :: a -> ListS a -> ListS a
consS x xss = ListS (\co _ -> co x xss)

infixr 5 `consS`
ass :: ListS Int
ass = 0 `consS` 1 `consS` 2 `consS` 3 `consS` nilS

r1 = sumS ass

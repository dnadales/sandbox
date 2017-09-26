To convert this file to Markdown use:

``` sh
pandoc Coercions.lhs -f markdown+lhs -t html -s -o Coercions.html
```

For more details see [these slides](https://johnmacfarlane.net/BayHac2014/#/).

> {-# LANGUAGE OverloadedLists            #-}
> {-# LANGUAGE TypeFamilies               #-}
> -- | Coercing a newtype into its subtype using lenses.
> module Coercions where
> 
> import           Data.Map.Strict (Map)
> import qualified Data.Map.Strict as Map
> import           GHC.Exts
> import           Data.List
> import           Data.Monoid
> import           Control.Lens.Wrapped
> import           Control.Lens.Setter

The problem
===========

We're working on a `FreeMonoid` data type, which represents structures of the
form:

```haskell
a0 <> a1 <> ... <> an-1
```

where `<>` represent the `mappend` monoid operation.

To be able to represent this, we use a `Map` that maps terms to the number of
times they occur in the expression:

> newtype FreeMonoid a = FreeMonoid { asMap :: Map a Int } deriving (Show)
>

Note that this representation requires that the `<>` operation is commutative.

We can represent free-monoids as lists:

> instance Ord a => IsList (FreeMonoid a) where
>     type Item (FreeMonoid a) = a
>     fromList xs = FreeMonoid $ Map.fromListWith (+) $ zip xs (repeat 1)
>     toList (FreeMonoid p) = do
>         (x, n) <- Map.toList p
>         genericReplicate n x
>

```haskell
ghci > [9, 2, 3, 4, 5, 1, 2, 4] :: FreeMonoid  Int
answ > FreeMonoid {asMap = fromList [(1,1),(2,2),(3,1),(4,2),(5,1),(9,1)]}
```

In the example above, the `<>` operation is not defined, so it is not a
free-monoid. Examples of free monoids include sums or products of integers:

> type FreeSum a = FreeMonoid (Sum a)
> type FreeProduct a = FreeMonoid (Product a)

Now we could define `fromList` and `toList` operations for `FreeSum` and
`FreeProduct` as follows:

```haskell
fromListSum :: Ord a => [a] -> FreeSum a
fromListSum = fromList . (Sum <$>)

fromListProduct :: Ord a => [a] -> FreeProduct a
fromListProduct = fromList . (Product <$>)
```

But this has quite a lot of boilerplate. It'd be nicer if we could simply say:

```haskell
fromListW :: (Ord a, Wrapper f) => [a] -> FreeMonoid (f a)
fromListW = fromList . (wrap <$>)
```

where `wrap` is some operation of the (hypotetical) `Wrapper` class were:

```haskell
wrap :: a -> f a
```

Similarly, we'd like to be able to write a function:
```haskell
toListW :: (Ord a, Wrapper f) => FreeMonoid (f a) -> [a]
toListW = (unwrap <$>) . toList
```

The question
------------
- Do lenses provide an abstraction similar to this `Wrapper` class?
- Can this "scrap-your-boilerplate" problem be solved by using lenses?

> fromListW :: (Ord a, Wrapped (f a))  => [a] -> FreeMonoid (f a)
> fromListW = fromList . (Wrapped <$>)
>
-- TODO: solve this. I asked a question here: https://stackoverflow.com/questions/46425567/how-to-eliminate-the-boilerplate-of-wrapping-and-unwrapping-using-lenses

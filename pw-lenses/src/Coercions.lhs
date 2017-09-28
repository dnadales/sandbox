To convert this file to Markdown use:

``` sh
pandoc Coercions.lhs -f markdown+lhs -t html -s -o Coercions.html
```

For more details see [these slides](https://johnmacfarlane.net/BayHac2014/#/).

> {-# LANGUAGE OverloadedLists       #-}
> {-# LANGUAGE TypeFamilies          #-}
> {-# LANGUAGE FlexibleInstances     #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
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
> import           Control.Lens.Getter
> import           Control.Lens.Iso

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

> newtype FreeMonoid a = FreeMonoid { asMap :: Map a Int } deriving (Eq, Show)
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

According to [this SO anwer](https://stackoverflow.com/questions/46425567/how-to-eliminate-the-boilerplate-of-wrapping-and-unwrapping-using-lenses), we can define `fromListW` as follows:

> fromListW :: (Ord a, Rewrapped a a)  => [Unwrapped a] -> FreeMonoid a
> fromListW = fromList . (Wrapped <$>)

Then you can readily use `fromListW` to produce the free-monoid of wrapped terms you want:

> mySumFM :: FreeMonoid (Sum Int)
> mySumFM = fromListW [2, 3, 4, 2, 4, 4]
> 
> myProductFM :: FreeMonoid (Product Int)
> myProductFM = fromListW [2, 3, 4, 2, 4, 4]

Two question that arises are:

1. Why does this work?
2. What if we want to have this automatic wrapping/unwrapping for our custom
   defined terms?

```haskell
instance (t ~ Sum b) => Rewrapped (Sum a) t
pattern Wrapped :: forall s. Rewrapped s s => Unwrapped s -> s 
```

Let's see what happens when we define out custom term types and try to obtains
free-monoids of them:

> newtype Tortilla a = Tortilla { getFilling :: a } deriving (Eq, Ord)
>
> instance Show a => Show (Tortilla a) where
>     show (Tortilla a) = "((( " ++ show a ++ " )))"

If we try:

```haskell
myWrapperFM :: FreeMonoid (Tortilla Int)
myWrapperFM = fromListW [2, 3, 4, 2, 4, 4]
```

We'll get an error saying:
```text
• No instance for (Rewrapped (Tortilla Int) (Tortilla Int))
    arising from a use of ‘fromListW’
• In the expression: fromListW [2, 3, 4, 2, ....]
  In an equation for ‘myWrapperFM’:
      myWrapperFM = fromListW [2, 3, 4, ....]
```

The [SO answer](https://stackoverflow.com/questions/46425567/how-to-eliminate-the-boilerplate-of-wrapping-and-unwrapping-using-lenses) also describe more lens-idiomatic implementations:

> fromListW2 :: (Wrapped a, Ord a) => [Unwrapped a] -> FreeMonoid a
> fromListW2 = fromList . view (mapping _Unwrapped')

Where we used these functions:
```haskell
mapping :: (Functor f, Functor g) => AnIso s t a b -> Iso (f s) (g t) (f a) (g b) 
view :: MonadReader s m => Getting a s a -> m a
_Unwrapped' :: Wrapped s => Iso' (Unwrapped s) s
_Wrapped' :: Iso' s (Unwrapped s) 
```

an `Iso` can be used as a `Getter`, however I don't know how the implementation
above works...

> mySumFM2 :: FreeMonoid (Sum Int)
> mySumFM2 = fromListW2 [2, 3, 4, 2, 4, 4]
> 
> myProductFM2 :: FreeMonoid (Product Int)
> myProductFM2 = fromListW2 [2, 3, 4, 2, 4, 4]

But we cannnot have:

```haskell
myIntFM :: FreeMonoid Int
myIntFM = fromListW2 [2, 3, 4, 2, 4, 4]
```

Since well get the error:
```text
• No instance for (Wrapped Int) arising from a use of ‘fromListW2’
• In the expression: fromListW2 [2, 3, 4, 2, ....]
  In an equation for ‘myIntFM’: myIntFM = fromListW2 [2, 3, 4, ....]
```

To go the other way around we can define:

> toListW :: (Wrapped a, Ord a) => FreeMonoid a -> [Unwrapped a]
> toListW = view (mapping _Wrapped') . toList

> mySumList = toListW mySumFM2
> myProductList = toListW myProductFM2

And to use `fromListW` and `toListW` in our custom wrapper data-types we have
to do the following:

> instance (t ~ Tortilla b) => Rewrapped (Tortilla a) t
> instance Wrapped (Tortilla a) where
>     type Unwrapped (Tortilla a) = a
>     _Wrapped' = iso getFilling Tortilla
>     {-# INLINE _Wrapped' #-}

> 
> myWrapperFM :: FreeMonoid (Tortilla Int)
> myWrapperFM = fromListW [2, 3, 4, 2, 4, 4]
> myFillingFM = toListW myWrapperFM

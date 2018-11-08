>{-# LANGUAGE DerivingVia #-}

Here are some examples of deriving QuickCheck instances.


> module DerivingQuickCheckInstances where
> import           Test.QuickCheck (Arbitrary)
> import           Test.QuickCheck.Modifiers
>   (Large (Large), NonNegative (NonNegative))
> import Data.Coerce (coerce)
> 
> newtype Duration = Duration Int
>   deriving Show
>   deriving Arbitrary via (NonNegative Int)
>
> newtype Duration2 = Duration2 Int
>   deriving Show
>   deriving Arbitrary via (NonNegative (Large Int))
>
>

You can't declare

```haskell
   deriving Arbitrary via (Large (NonNegative Int))
```

since this will result in the following error:

```text
     • No instance for (Integral (NonNegative Int))
        arising from the 'deriving' clause of a data type declaration
      Possible fix:
        use a standalone 'deriving instance' declaration,
          so you can specify the instance context yourself
    • When deriving the instance for (Arbitrary Duration2)
   |
17 |   deriving Arbitrary via (Large (NonNegative Int))
   |            ^^^^^^^^^
```

It is still not clear why you cannot compose this in the oposite order.

```haskell
instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a)
instance (Integral a, Bounded a) => Arbitrary (Large a)
```

If you look at the way the NonNegative instance is defined, it is easier to
see how these generators compose:

```haskell
instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a) where
arbitrary =
  (frequency
     why is this distrbution like this?
     [ (5, (NonNegative . abs) `fmap` arbitrary)
     , (1, return (NonNegative 0))
     ]
  ) `suchThat` ge0
  where ge0 (NonNegative x) = x >= 0
```

It just takes an arbitrary `a` and computes the `abs` value.

Non-well typed uses of `deriving via`
-------------------------------------

```haskell
newtype S = S Char
  deriving Eq via Maybe

```

The code above will result in
```text
    • Cannot derive instance via ‘Maybe’
        Class ‘Eq’ expects an argument of kind ‘*’,
        but ‘Maybe’ has kind ‘* -> *’
    • In the newtype declaration for ‘S’
   |
70 | >   deriving Eq via Maybe
   |              ^^

```

```haskell
newtype S = S Char
  deriving Eq via Maybe Char
```

The code above will result in:
```text
    • Couldn't match representation of type ‘Char’
                               with that of ‘Maybe Char’
        arising from the coercion of the method ‘==’
          from type ‘Maybe Char -> Maybe Char -> Bool’
            to type ‘S -> S -> Bool’
    • When deriving the instance for (Eq S)
   |
91 | >  deriving Eq via Maybe Char
   |             ^^
```

Coercible
---------

> duration :: Int -> Duration
> duration = coerce

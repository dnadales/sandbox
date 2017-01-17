Exercises on folds
==================

> module Chapter03.Folds where
>
> import Prelude hiding (foldl)

Implementing `foldl` with `foldr`
---------------------------------

In this section we use the results presented in the paper [A tutorial on the
universality and expressiveness of fold](www.cs.nott.ac.uk/~pszgmh/fold.pdf).
Accordingly, `fold` is used as a synonim for `foldr`.

The universal property for `fold` states that:

```haskell
g [] = v
g (x:xs) = h x (g xs)
```

if and only if

```haskell
g = fold h v
```

Now consider the definition of `foldl`:

> foldl :: (a -> b -> a) -> a -> [b] -> a
> foldl f z [] = z
> foldl f z (x:xs) = foldl f (f z x) xs

Let's try this on some examples:

> xs = "foo bar"
> zs = foldl (flip (:)) [] xs
> -- Compare with the effect of applying `foldr`:
> ys = foldr (:) [] xs

Can we define `foldl` using `foldr` using the universal property? Replacing `g`
by `foldl f z` in the terms of the property we get:

```haskell
(foldl f z) [] = v
(foldl f z) (x:xs) = h x ((foldr f z) xs)
```

if and only if

```haskell
foldl f z = fold h v
```

Now to define `foldl` in terms of `foldr` we need to find `v` and `h` above.
By definition of `foldl` we know that:

```haskell
(foldl f z) []
-- ={ def. of fold } 
[]
```

Then we have `v = []`. To find `h` we can expand the second equation:

```haskell
(foldl f z) (x:xs)
-- ={ def. of fold } 
foldl f (f z x) xs
```

It seems we cannot put `foldl f (f z x) xs` in the form `h x ((foldr f z) xs)`.
The problem seems to be that the second argument of `foldl` is causing trouble.
So we could define:

```haskell
foldl' f xs v = foldl f v xs
```

Now using the universal property of `fold` we need to find `v` and `h` such
that:

```haskell
(foldl' f) [] = v
(foldl' f) (x:xs) = h x ((foldl' f) xs)
```

For the base case we have:

```haskell
(foldl' f) []
-- = {lambda abstraction}
\x -> (foldl' f) [] x
-- = {def. foldl'}
\x -> foldl f x []
-- = {def. foldl}
\x -> x
-- {def. id}
id
```

Then, we have `v = id`.

For the inductive case consider:

```haskell
(foldl' f) (x:xs)
-- = {lambda abstraction}
\y -> (foldl' f) (x:xs) y
-- = {def. foldl'}
\y -> (foldl f) y (x:xs)
-- = {def. foldl}
\y -> (foldl f) (f y x) xs
-- = {def. foldl'}
\y -> (foldl' f) xs (f y x)
-- = {lambda abstraction}
(\z -> \y -> (foldl' f) xs (f y z)) x
-- = {lambda abstraction}
(\z w -> \y -> w (f y z)) x ((foldl' f) xs)
```

Then making `h = (\z w -> \y -> k w (f y z))}` we have:


> foldl' :: (a -> b -> a) -> [b] -> a -> a
> foldl' f = foldr (\z w -> \y -> w (f y z)) id
>
> foldlViaFold :: (a -> b -> a) -> a -> [b] -> a
> foldlViaFold f v xs = (foldr (\z w -> \y -> w (f y z)) id) xs v

> zs' = foldlViaFold (flip (:)) [] xs

Can we define `foldr` in terms of `foldl`
----------------------------------------

Only for finite lists! For some context and additional information see [this SO
question](http://stackoverflow.com/questions/41686292/why-it-is-not-possible-to-redefine-implement-foldr-in-terms-of-foldl).

> foldr' :: (a -> b -> b) -> b -> [a] -> b
> foldr' f v xs = foldl (\g u x -> g (f u x)) id xs v
> -- And let's try this with some examples:
> ys' = foldr' (:) [] xs
> ex0 = foldr (+) 0 [0..20]
> ex0' = foldr' (+) 0 [0..20]

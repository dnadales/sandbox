Excercise 3.15
--------------

Write a function that concatenates a list of lists into a single list. Its
runtime should be linear in the total lenght of all lists.

> module Chapter03.Concat where

First let's try with the naive version:

> -- | N is for naive.
> concatN :: [[a]] -> [a]
> concatN [] = []
> concatN (x:xs) = x ++ concatN xs

Let's try this on some examples:

> mkLists n = [[0 .. i] | i <- [0 .. n]]

Let's try the linear version:

> concatL :: [[a]] -> [a]
> concatL [] = []
> concatL ([]:yss) = concatL yss
> concatL ((x:xs):yss) = x : (concatL (xs:yss))

This is not working as I would expect!

```
ghci> length $ concatN (mkLists 10000)
50015001
ghci> length $ concatL (mkLists 10000)
-- Takes a long time!
50015001
```

What about a tail-recursive implementation of `concat`?

> concatLT :: [[a]] -> [a]
> concatLT xs = concatLT' [] xs
>   where concatLT' acc [] = acc
>         concatLT' acc ([]: yss) = concatLT' acc yss
>         concatLT' acc ((x:xs):yss) = concatLT' (x:acc) (xs:yss)
>

It is still slow!

Let's try with some auxiliary functions...

>
> append :: [a] -> [a] -> [a]
> append xs ys = foldr ((:)) ys xs
>
> concat3 :: [[a]] -> [a]
> concat3 xss = foldr (append) [] xss

Can we benchmark this?


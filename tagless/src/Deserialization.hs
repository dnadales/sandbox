{-# LANGUAGE RankNTypes #-}
module Deserialization where

import           Control.Monad (liftM2)
import           Text.Read     (readEither)

import           ExpSYM


data Tree = Leaf String
          | Node String [Tree]
          deriving (Eq, Read, Show)

instance ExpSYM Tree where
    lit n = Node "Lit" [Leaf (show n)]
    neg e = Node "Neg" [e]
    add e0 e1 = Node "Add" [e0, e1]

-- | Serialization of a 'ExpSYM' via its instance.
toTree :: Tree -> Tree
toTree = id

--------------------------------------------------------------------------------
-- De-serialization
--------------------------------------------------------------------------------

type ErrMsg = String

fromTree :: ExpSYM repr => Tree -> Either ErrMsg repr
fromTree (Node "Lit" [Leaf n]) = lit <$> readEither n
fromTree (Node "Neg" [e])      = neg <$> fromTree e
fromTree (Node "Add" [e0, e1]) = liftM2 add (fromTree e0) (fromTree e1)

tf1Tree :: Tree
tf1Tree = toTree tf1

-- Then you can use 'fromTree' to de-serialize a tree into the desired representation:
--
-- > eval <$> fromTree tf1Tree
-- > Right 5
-- > it :: Either ErrMsg Int
--
-- > view <$> fromTree tf1Tree
-- > Right "(8 + - ((1 + 2)))"
-- > it :: Either ErrMsg String
--

-- But there's a problem.... we cannot use 'eval' and 'view' after decoding!
evalAndShow :: IO ()
evalAndShow =
    case fromTree tf1Tree of
        Left e     -> putStrLn $ "Error: " ++ e
        Right repr -> do
            print $ eval repr -- Here @repr@ will be bound to 'String', so we cannot use 'view'!
--             print $ view repr

-- We can fake the polymorphism as follows:
newtype Wrapped = Wrapped (forall repr . ExpSYM repr => repr)

fromTreeW :: Tree -> Either ErrMsg Wrapped
fromTreeW (Node "Lit" [Leaf n]) = do
    r <- readEither n
    return $ Wrapped (lit r)
fromTreeW (Node "Neg" [e]) = do
    Wrapped r <- fromTreeW e
    return $ Wrapped (neg r)
fromTreeW (Node "Add" [e0, e1]) = do
    Wrapped r0 <- fromTreeW e0
    Wrapped r1 <- fromTreeW e1
    return $ Wrapped (add r0 r1)


evalAndShowW :: IO ()
evalAndShowW =
     -- QUESTION TO SO: fromTreeW should be evaluated only once, right? How can
     -- this be checked?
    case fromTreeW tf1Tree of
        Left e     -> putStrLn $ "Error: " ++ e
        Right (Wrapped repr) -> do
            print $ eval repr -- Here @repr@ will be bound to 'String', so we cannot use 'view'!
            print $ view repr

--------------------------------------------------------------------------------
-- Solution with "The puzzling interpreter"
--------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}
module Deserialization where

import           Control.Monad (liftM2)
import           Data.Foldable (traverse_)
import           Data.Function (fix)
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
fromTree e                     = Left $ "Invalid tree: " ++ show e

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

-- | A duplicating interpreter.
instance (ExpSYM repr, ExpSYM repr') => ExpSYM (repr, repr') where
    lit x = (lit x, lit x)
    neg (e0, e1) = (neg e0, neg e1)
    add (le0, le1) (re0, re1) = (add le0 re0, add le1 re1)

duplicate :: (ExpSYM repr, ExpSYM repr') => (repr, repr') -> (repr, repr')
duplicate = id

-- Now, how do we use this function to write a function 'thrice' that evals,
-- prints, and encodes an expression?

thrice :: (Int, (String, Tree)) -> IO () -- Is this the most generic type we can give?
thrice x0 = do
    x1 <- dupConsume eval x0
    x2 <- dupConsume view x1
    print $ toTree x2
    where
      dupConsume ev y = do
          print (ev y0)
          return y1
              where
                (y0, y1) = duplicate y

-- See https://stackoverflow.com/questions/51457533/typed-tagless-final-interpreters-what-is-the-use-of-duplicate
thrice' :: (Int, (String, Tree)) -> IO ()
thrice' (reprInt, (reprStr, reprTree)) = do
    print $ eval reprInt
    print $ view reprStr
    print $ toTree reprTree

printTrice :: IO ()
printTrice = traverse_ thrice' (fromTree tf1Tree)

-- | Extension of the deserializer using the open recursion style.
fromTreeExt :: (ExpSYM repr)
            => (Tree -> Either ErrMsg repr) -> (Tree -> Either ErrMsg repr)
fromTreeExt _    (Node "Lit" [Leaf n]) = lit <$> readEither n
fromTreeExt self (Node "Neg" [e])      = neg <$> self e
fromTreeExt self (Node "Add" [e0, e1]) = liftM2 add (self e0) (self e1)
fromTreeExt _    e                     = Left $ "Invalid tree: " ++ show e

fromTree' :: ExpSYM repr => Tree -> Either ErrMsg repr
fromTree' = fix fromTreeExt

-- TODO: prove that fromTree' == fromTree

-- Run the examples:
tf1EInt3 :: IO ()
tf1EInt3 = traverse_ thrice (fromTree' tf1Tree)

tfxEInt3 :: IO ()
tfxEInt3 = traverse_ thrice (fromTree' wrongTree)
    where
      wrongTree = Node "Lit" [Leaf "0", Leaf "2"]

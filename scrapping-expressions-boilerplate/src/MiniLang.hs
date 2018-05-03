{-# LANGUAGE DeriveDataTypeable #-}
module MiniLang where

import           Control.Lens        ((^.))
import           Control.Lens.Fold   ((^..))
import           Control.Lens.Setter (over)
import           Data.Data
import           Data.Data.Lens      (biplate, uniplate)

data Tree a = Leaf a
            | Fork (Tree a) (Tree a)
            deriving (Data, Show, Eq)

t0 = Fork (Fork (Leaf "foo") (Leaf "bar")) (Leaf "baz")

t1 = Fork t0 t0

t2 = Fork (Fork t0 t0) (Fork (Leaf "zero") (Leaf "one"))

reverseT :: Data a => Tree [a] -> Tree [a]
reverseT (Leaf xs) = Leaf (reverse xs)
reverseT xt        = over uniplate reverseT xt

leavesT :: Data a => Tree a -> [a]
leavesT xt       = xt ^.. biplate

data Bexp = Stop
          | Cref Chan
          | Par Bexp Bexp
    deriving (Data, Show, Eq)

data Chan = A
          | B
          | S Special
    deriving (Data, Show, Eq)

newtype Special = Special String
    deriving (Data, Show, Eq)

chans :: Bexp -> [Chan]
chans be = be ^.. biplate

specials :: Bexp -> [Special]
specials be = be ^.. biplate

be = Par
    (Par
        (Par Stop (Par (Cref A) (Cref (S (Special "Another one ...")))))
        (Cref A))
    (Par
        (Cref B)
        (Par Stop
            (Cref (S (Special "I am..."))
            )))

c0 = S (Special "hello!")

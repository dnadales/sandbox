module Exp where

data Exp = Lit Int
         | Neg Exp
         | Add Exp Exp
         deriving (Eq, Show)

ti1 = Add (Lit 8) (Neg (Add (Lit 1) (Lit 2)))

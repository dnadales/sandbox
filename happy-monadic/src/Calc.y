{
    
module Calc
  ( calc
  , lexer
  , parse
  ) where

import Data.Char
import Control.Monad.Reader

}

%name calc

%tokentype { Token }

%error { parseError }

%monad { ExpParser }

%token 
  let { TokenLet }
  in  { TokenIn }
  int { TokenInt $$ }
  var { TokenVar $$ }
  '=' { TokenEq }
  '+' { TokenPlus }
  '-' { TokenMinus }
  '*' { TokenTimes }
  '/' { TokenDiv }
  '(' { TokenOB }
  ')' { TokenCB }

%%

Exp :: { Exp }
    : let var '=' Exp in Exp {% ask >>= \f -> return $ Let (f $2) $4 $6 }
    | Exp1                   { id $1 }

Exp1 :: { Exp }
     : Exp1 '+' Term { Plus $1 $3 }
     | Exp1 '-' Term { Minus $1 $3 }
     | Term          { id $1}

Term :: { Exp }
     : Term '*' Factor { Times $1 $3 }
     | Term '/' Factor { Div $1 $3 }
     | Factor          { id $1 }

Factor :: { Exp }
       : int         { Int $1 }
       | var         {% ask >>= \f -> return $ Var (f $1) }
       | '(' Exp ')' { id $2 }

{

data Exp = Let String Exp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Var String
         | Int Int
         deriving Show

-- | Tokens
data Token
      = TokenLet
      | TokenIn
      | TokenInt Int
      | TokenVar String
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenOB
      | TokenCB
 deriving Show

-- | Parser
newtype ExpParser a = ExpParser { runExpParser :: Reader (String -> String) a }
  deriving (Functor, Applicative, Monad, MonadReader (String -> String))

-- | Error
parseError :: [Token] -> ExpParser a
parseError t = error $ "Parse error: " ++ (show t)

-- | Parsing function.
parse :: (String -> String) -> String -> Exp
parse f = (`runReader` f) . runExpParser . calc . lexer

-- * Lexer functions (TODO: integrate with Alex)
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      ("let",rest) -> TokenLet : lexer rest
      ("in",rest)  -> TokenIn : lexer rest
      (var,rest)   -> TokenVar var : lexer rest

}    

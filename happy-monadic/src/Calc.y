{
    
module Calc
  ( calc
  , lexer
  , parse
  ) where

import Data.Char
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Lexer

}

%name calc

%tokentype { Token }

%error { parseError }

%monad { ExpParser }

%lexer { mLexer } { TokenEOF }

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
    : let var '=' Exp in Exp
    {% ask >>= \env -> return $ Let ((varModifier env) $2) $4 $6 }
    | Exp1                   { $1 }

Exp1 :: { Exp }
     : Exp1 '+' Term { Plus $1 $3 }
     | Exp1 '-' Term { Minus $1 $3 }
     | Term          { $1 }

Term :: { Exp }
     : Term '*' Factor { Times $1 $3 }
     | Term '/' Factor { Div $1 $3 }
     | Factor          { $1 }

Factor :: { Exp }
       : int         { Int $1 }
       | var
       {% ask >>= \env -> return $ Var ((varModifier env) $1) }
       | '(' Exp ')' { $2 }

{

data Exp = Let String Exp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Var String
         | Int Int
         deriving Show

-- | Parser
data ParserEnv = ParserEnv
  { input :: String
  , varModifier :: String -> String
  }

newtype ExpParser a = ExpParser 
  { runExpParser :: ReaderT ParserEnv (StateT AlexPosn (Except String) a) }
  deriving ( Functor, Applicative, Monad
           , MonadReader ParserEnv
           , MonadState AlexPosn
           , MonadError String
           )

-- | Error
parseError :: Token -> ExpParser a
parseError t = throwError $ "Parse error: " ++ (show t)

-- | Parsing function.
parse :: (String -> String) -> String -> Either String Exp
parse f str = runExcept $ (`evalStateT` initState) $ runReaderT (runExpParser calc) initEnv
  where initEnv = ParserEnv { input = str, varModifier = f}
        initState = AlexPn 0 0 0

-- * Lexer functions (TODO: integrate with Alex)
mLexer :: (Token -> ExpParser a) -> ExpParser a
mLexer cont = do
  input <- asks input
  let token = alexScanTokens posn input
  cont token

-- alexScanTokens :: AlexPosn -> String -> Token

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

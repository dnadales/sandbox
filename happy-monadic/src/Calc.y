{
    
module Calc
  ( calc
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
  { varModifier :: String -> String }


-- TODO: consider simplifying this by using only the `Alex` monad!
newtype ExpParser a = ExpParser 
  { runExpParser :: ReaderT ParserEnv (StateT AlexState (Except String)) a }
  deriving ( Functor, Applicative, Monad
           , MonadReader ParserEnv
           , MonadState AlexState
           , MonadError String
           )

-- | Error
parseError :: Token -> ExpParser a
parseError t = throwError $ "Parse error: " ++ (show t)

-- | Parsing function.
parse :: (String -> String) -> String -> Either String Exp
parse f str = runExcept $ (`evalStateT` initState) $ runReaderT (runExpParser calc) initEnv
  where initEnv = ParserEnv { varModifier = f}
        initState = AlexState -- TODO: isn't it a standard initial state that we can use?        
          { alex_pos = AlexPn 0 0 0
          , alex_inp = str
          , alex_chr = '\n' -- TODO: What to include here?
          , alex_bytes = []
          , alex_scd = 0
          }

-- * Lexer functions (TODO: integrate better with Alex)
mLexer :: (Token -> ExpParser a) -> ExpParser a
mLexer cont = do -- TODO: is there a way to reduce this boilerplate?
  alexSt <- get
  case unAlex alexMonadScan alexSt of
    Left err -> throwError err
    Right (nextAlexSt, token) ->
      do
        put nextAlexSt
        cont token

}    

{
module Lexer
  ( runAlex
  , alexMonadScan
  , alexEOF
  , Token (..)
  , AlexState (..)
  , AlexPosn (..)
  , Alex (..)
  ) where  
}

%wrapper "monad"

$digit = 0-9

calcTokens :-

  $digit+ { tokenInt  }

{
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
      | TokenEOF
 deriving (Eq, Show)

tokenInt :: AlexInput -> Int -> Alex Token
tokenInt (_, _, _, s) _ = return $ TokenInt (read s)

alexEOF :: Alex Token
alexEOF = return TokenEOF

}

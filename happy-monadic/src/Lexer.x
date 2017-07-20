{
module Lexer
  ( alexScanTokens
  , Token (..)
  , AlexPosn
  ) where  
}

%wrapper "posn"

$digit = 0-9

calcTokens :-

  $digit+ { \_ s -> TokenInt (read s) }
  .       { \ _  _ -> Err }

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
      | Err
 deriving (Eq, Show)

}

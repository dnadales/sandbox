module TypedFormatting where

import           Prelude hiding ((^))

-- | Formatting specifications.
--
-- >>> :t lit "hello "
-- lit "hello " :: FormattingSpec repr => repr a a
--
-- >>> :t lit "hello " ^ lit "world"
-- lit "hello " ^ lit "world" :: FormattingSpec repr => repr c c
--
-- >>> :t lit "hello " ^ lit "world" ^ char
-- lit "hello " ^ lit "world" ^ char
--   :: FormattingSpec repr => repr a (Char -> a)
--
-- >>> :t lit "hello " ^ int ^ lit "worlds"
-- lit "hello " ^ int ^ lit "worlds"
--   :: FormattingSpec repr => repr b (Int -> b)
--
-- >>> :t int ^ char ^ int
-- int ^ char ^ int
--   :: FormattingSpec repr => repr a (Int -> Char -> Int -> a)
--
class FormattingSpec repr where
    -- | NOTE: @repr a a@ represents the identity functor.
    lit :: String -> repr a a
    -- | NOTE: @repr a (Int -> a)@ represents the @Int -> a@  functor.
    int :: repr a (Int -> a)
    char :: repr a (Char -> a)
    (^) :: repr b c -> repr a b -> repr a c

infixl 5 ^

--------------------------------------------------------------------------------
-- Interpreters
--------------------------------------------------------------------------------

newtype FPr a b = FPr ((String -> a) -> b)

instance FormattingSpec FPr where
    lit str = FPr $ \f -> f str

    -- int :: FPr a (Int -> a) ~~ (String -> a) -> (Int -> a)
    int = FPr $ \f -> \i -> f (show i)

    char = FPr $ \f -> \c -> f [c]

    -- (^) :: FPr b c -> FPr a b -> FPr a c
    --     ~~
    --     :: ((String -> b) -> c) -> ((String -> a) -> b) -> ((String -> a) -> c)
    FPr g ^ FPr f = FPr $ \sa ->
        g (\s -> f (\s' -> sa (s ++ s')))

-- | Remember that @FPr String b@ is isomorphic to @((String -> String) -> b)@.
--
-- Some examples:
--
-- >>> :t sprintf (lit "hello " ^ int ^ lit " worlds")
-- sprintf (lit "hello " ^ int ^ lit " worlds") :: Int -> String
--
-- >>> :t sprintf (lit "hello " ^ int ^ lit " worlds" ^ int )
-- sprintf (lit "hello " ^ int ^ lit " worlds" ^ int )
--   :: Int -> Int -> String
--
sprintf :: FPr String b -> b
sprintf (FPr f) = f id

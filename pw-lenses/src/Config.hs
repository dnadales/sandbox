{-# LANGUAGE RankNTypes #-}
-- | Example of using lenses for manipulating a configuration file.

module Config where

import           Control.Lens

data Config = Config
  { verbose :: Bool
  , authors :: [Author]
  , foo     :: Foo
  } deriving (Show)

data Author = Author
  { name :: String
  , age  :: Int
  } deriving (Show)

newtype Foo = Foo { bar :: String } deriving Show

-- * Lenses for `Config` and `Author`.

verboseL :: Lens' Config Bool
-- Remember that a lens is defined as:
--
-- > Lens' Config Bool = forall f . Functor f => (Bool -> f Bool) -> Config -> f Config
--
verboseL fb cfg = (\b -> cfg { verbose = b } ) <$> fb (verbose cfg)

authorsL :: Lens' Config [Author]
authorsL fxs cfg = (\xs -> cfg { authors = xs }) <$> fxs (authors cfg)

fooL :: Lens' Config Foo
fooL ff cfg = (\f -> cfg { foo = f }) <$> ff (foo cfg)

nameL :: Lens' Author String
nameL fstr a = (\str -> a { name = str }) <$> fstr (name a)

ageL :: Lens' Author Int
ageL fi a = (\i -> a { age = i }) <$> fi (age a)

barL :: Lens' Foo String
barL fstr fo = (\str -> fo { bar = str} ) <$> fstr (bar fo)

-- * Sample configurations.
config0 :: Config
config0 = Config
  { authors = [nicole, another], verbose = False , foo = baz}
  where nicole = Author "One" 35
        another = Author "Two" 27
        baz = Foo "baz"

-- * Playing with lenses

foo0 :: String
foo0 = view (fooL . barL) config0

-- How come we're able to compose lenses?
--
-- We have:
--
-- > (.) :: Lens' Config Foo -> Lens' Foo String -> Lens' Config String
--
-- Expanding we get (ignore the universal quantification for now ...):
--
-- > (.) :: (forall f. Functor f => (Foo -> f Foo) -> Config -> f Config)
--       -> (forall f. Functor f => (String -> f String) -> Foo -> f Foo)
--       -> (forall f. Functor f => (String -> f String) -> Config -> f Config)
--
-- Which coincides with the usual definition of function composition:
--
-- > (.) :: (b -> c) -> (a -> b) -> (a -> c)

-- What is the type of `view`?
--
-- > view :: MonadReader s m => Getting a s a -> m a
-- >         MonadReader s m => ((a -> Const a a) -> s -> Const a s) -> m a
--
-- Instantiated for our particular case (`foo0`):
--
-- > view :: MonadReader Config m => ((String -> Const String String) -> Config -> Const String Config) -> m String
--
-- And in this case the monad reader is `m ~ (Config ->) String` or
-- equivalently `m ~ Config -> String`.

authorNames0 :: [String]
-- The implementation below won't typecheck. Traverse will apply the Monoid
-- operator on strings (resulting on a value `String`), whereas we want a list
-- of names! (of type `[String]`)
--
-- > authorNames0 = view (authorsL . traverse . nameL) config0
--
-- How is `traverse` a lens? Remember its type:
--
-- > traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
--
-- And compare with
--
-- > Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
--
-- So a traverse it is not a `Lens'` but a `Lens (t a) (t b) a b`!
authorNames0 = toListOf (authorsL . traverse . nameL) config0

-- TODO: understand the `toList` and `Traversal` magic:
--
-- > https://hackage.haskell.org/package/lens-tutorial-1.0.3/docs/Control-Lens-Tutorial.html#g:5

authorAges0 :: [Int]
authorAges0 = toListOf (authorsL . traverse . ageL) config0

-- * Parametrizing lenses.

-- | Append a string to the field defined by the given Traversal. Note that we
-- could have used a Lens instead, but with Traversal we can use traversal:
--
-- > appendPorAtras (authorsL . traverse . nameL) config0
--
-- Besides of course:
--
-- > appendPorAtras (fooL . barL) config0
--
appendPorAtras :: forall a . Traversal' a String -> a -> a
appendPorAtras accessor = over accessor (++" por atras")

-- Why does `traverse` works as a Traversal?
--
-- > authorsL :: Functor f => ([Author] -> f [Author]) -> Config -> f Config
-- > authorsL . traverse :: Applicative f => (Author -> f Author) -> Config -> f Config
-- > authorsL . traverse . nameL :: Applicative f => (String -> f String) -> Config -> f Config
--


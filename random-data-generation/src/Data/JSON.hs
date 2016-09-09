-- | A stripped down version of JSON.

module Data.JSON where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.Functor.Identity
import           Data.List
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Test.QuickCheck          hiding (maxSize)

data JValue = JNull
            | JString String
            | JNumber Double
            | JBool Bool
            | JObject [(String, JValue)]
            | JArray  [JValue]
            deriving (Show)

-- * Some helper functions

-- | Gets the top level
attributes :: JValue -> [String]
attributes (JObject xs) = fst <$> xs
attributes _ = []

-- Note that the instance of Arbitrary has to be declared here:
--     http://stackoverflow.com/questions/3079537/orphaned-instances-in-haskell

-- * Our first approach:
--
instance Arbitrary JValue where
  arbitrary = oneof [ return JNull
                    , JString <$> arbitrary
                    , JBool <$> arbitrary
                    , JObject <$> (listOf $ liftM2 (,) arbitrary arbitrary)
                    , JArray <$> (listOf arbitrary)
                    ]
--
--
-- You can see the attributes that were generated by performing:
--
-- > attributes <$> (generate  (arbitrary :: Gen JValue))
--

-- * Second approach:

class Configuration c where
  -- | The universe of valid strings.
  validString :: c -> Gen String
  -- | The maximum size of the generated objects.
  maxSize :: c -> Int
  -- | Update the maximum size.
  updateMaxSize :: (Int -> Int) -> c -> c

data Conf = Conf { _validStrings :: [String]
                 , _maxSize      :: Int
                 }

instance Configuration Conf where
  validString c = elements (_validStrings c)
  maxSize c = _maxSize c
  updateMaxSize f c = c {_maxSize = (f . _maxSize) c}

mConf :: Conf
mConf = Conf ["foo", "bar", "baz"] 4

arbitraryC :: (Configuration c) => c -> Gen JValue
arbitraryC conf =
  if (n == 0)
  then return JNull
  else oneof [ return JNull
             , liftM JString validStr
             , liftM JBool arbitrary
             , liftM JObject (vectorOf n $ liftM2 (,) validStr (arbitraryC conf'))
             , liftM JArray (vectorOf n (arbitraryC conf'))
             ]
  where validStr = validString conf
        n = maxSize conf
        conf' = updateMaxSize (\x -> x - 1) conf

type GenE c a = ReaderT c Gen a

-- * Using the reader Monad to pass the environment around.
arbitraryE :: (Configuration c) => GenE c JValue
arbitraryE =
  asks (maxSize) >>= \n -> -- Here we ask the max size to the environment ::
                           -- ReaderT c Gen Int
  asks (validString) >>= \validStr ->
    if (n == 0)
    then return (JBool True)
    else local (updateMaxSize (+(-1))) $
         mapReaderT (\gen -> oneof [ return JNull
                                   , liftM JString validStr
                                   , liftM JBool arbitrary
                                   , liftM JObject (vectorOf n $ liftM2 (,) validStr (gen))
                                   , liftM JArray (vectorOf n (gen))
                                   ]) $
         arbitraryE


-- Now we have a lot of complexity! Is it worth it?

getRandomJSON ::  IO JValue
getRandomJSON = generate $ runReaderT arbitraryE mConf

-- To print a value use:
--
-- > getRandomJSON >>= putStrLn . show
--

-- Ok, next, we want to change the behavior, so that strings are not repeated!

-- * State
-- class GenState s where
--   -- | Returns a string from the state, and removes the string from the pool of
--   -- available strings.
--   getString :: s -> (String, s)

newtype GenState = St {getStrings :: [String]}
  deriving (Show)

mState :: GenState
mState = St ["foo", "bar", "baz", "fuss"]

-- |  Removes a String from the state.
removeString :: String -> GenState -> GenState
removeString str (St xs) = St $ delete str xs

-- ** First attempt

-- Can we define a function:
stringGenS ::  Gen (a, GenState) -> Gen (String, GenState)
-- This is a bit ackward because I'm ignoring the first element returned by the
-- generator.
stringGenS genStSt =
  genStSt >>= \(_, st) ->
  elements (getStrings st) >>= \str ->
  return (str, removeString str st)


arbitraryStringS :: StateT GenState Gen String
-- arbitraryStringS =
--   mapStateT stringGenS get

-- Answer given at http://goo.gl/oy9Zyc:
--
-- > arbitraryStringS = do
-- >   s <- get
-- >   str <- lift $ elements (getStrings s)
-- >   modify $ removeString str
-- >   return str
--
-- Desugared:
arbitraryStringS =
  get >>= \st ->
  (lift $ elements (getStrings st)) >>= \generatedStr ->
  modify (removeString generatedStr) >>
  return generatedStr


genTwoStrings :: StateT GenState Gen (String, String)
genTwoStrings = liftM2 (,) arbitraryStringS arbitraryStringS

getRandomStr :: IO (String, GenState)
getRandomStr = generate $ runStateT arbitraryStringS mState

getTwoRandomStrs :: IO ((String, String), GenState)
getTwoRandomStrs = generate $ runStateT genTwoStrings mState

showRandomStr :: IO ()
showRandomStr = getRandomStr >>= putStrLn . show

showTwoRandomStr :: IO ()
showTwoRandomStr = getTwoRandomStrs >>= putStrLn . show

threeRandomStr :: StateT GenState Gen [String]
threeRandomStr =
  sequence [arbitraryStringS, arbitraryStringS, arbitraryStringS]

showThreeRandomStr :: IO ()
showThreeRandomStr = (generate $ runStateT threeRandomStr mState) >>= putStrLn .show
-- Cool, we can generate random strings. Could we also generate random JValues?

arbitraryS :: StateT GenState Gen JValue
-- Next: choose between JString and JNull

-- And then: add JArray

-- And then: handle the case of an exhausted resource pool (maybe generating
-- only what we can...).
--
-- I would like to use `oneof`
-- oneof :: [Gen a] -> Gen a

-- If we didn't use the state we could readyly make use of `oneof`
-- arbitraryS = lift $ oneof [return JNull, liftM JString arbitrary]

-- But we want to use our string generator.
-- Can we use `oneof` here?
arbitraryS =
  oneofS [return JNull
         , JString <$> arbitraryStringS
         , JArray <$> listSOf arbitraryS
         , JObject <$> (listSOf $ liftM2 (,)  arbitraryStringS arbitraryS)
         ]

-- What we need is our stateful version of oneof:
oneofS :: [StateT GenState Gen JValue] -> StateT GenState Gen JValue
oneofS gens = do
  n <- lift $ elements [0 .. length gens - 1]
  gens !! n

-- Our stateful version of listof:
listSOf :: StateT GenState Gen a -> StateT GenState Gen [a]
listSOf gen = gListSOf [] gen

gListSOf :: [a] -> StateT GenState Gen a -> StateT GenState Gen [a]
gListSOf xs gen = do
  p <- lift $ (choose (0.0, 1.0) :: Gen Double)
  if p < 0.1  -- TODO: this probability should be read from the environment in the future.
    then return xs
    else do
    x <- gen
    gListSOf (xs ++ [x]) gen

--  elements [lift $ return JNull, JString <$> arbitraryStringS]
--  >>= \gens -> lift $ elements  gens

arbitraries :: StateT GenState Gen [JValue]
arbitraries =
  sequence [ return JNull
           , arbitraryStringS >>= \str -> return (JString str)
           ]

showRandomJSon :: IO ()
showRandomJSon = (generate $ runStateT arbitraryS  mState) >>= putStrLn . show

-- Cool! We have a working version of our generator. However, we see that we
-- get an error if we run out of objects in the objects pool. In this case, we
-- could just generate a JNull value!
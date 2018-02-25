-- |
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module SimpleADTs where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Lens.Micro
import           GHC.Generics (Generic)
import           GHC.Exts
import           Data.List hiding (sort)

-- * Our basic data-types.

newtype ADTs = ADTs
    { adtsToMap :: Map Name (ADT Sort)
    } deriving (Eq, Show)

data ADT t = ADT
    { _adtName :: Name
    , _constructors :: Constructors t
    } deriving (Eq, Show)

newtype Name = Name String deriving (Eq, Ord, Show, Generic)

instance IsString Name where
    fromString = Name

newtype Constructors t = Constructors
    { constructorsToMap :: Map Name (Constructor t)
    } deriving (Eq, Show)

data Constructor t = Constructor
    { _constructorName :: Name
    , _constructorFields :: Fields t
    } deriving (Eq, Show)

newtype Fields t = Fields
    { fieldsToList :: [Field t]
    } deriving (Eq, Show)

data Field t = Field
    { _fieldName :: Name
    , _sort :: t
    } deriving (Eq, Show)

newtype Sort = Sort String deriving (Eq, Show)

-- * Some utility classes and functions to make our life easier

-- First let's define some infix operators to make our life easier:
--

-- | Class to pretty print an ADT and its parts:
class ShowPretty a where
    showPretty :: a -> String
    showPretty = showPrettyIndent ""

    showPrettyIndent :: String -> a -> String

instance ShowPretty String where
    showPrettyIndent xs = (xs ++) . id

instance ShowPretty Name where
    showPrettyIndent xs (Name n) = xs ++ n

instance ShowPretty Sort where
    showPrettyIndent xs (Sort n) = xs ++ "<" ++ n ++ ">"

instance ShowPretty t => ShowPretty (Field t) where
    showPrettyIndent xs (Field n s) = xs ++ showPretty n ++ ": " ++ showPretty s

instance ShowPretty t => ShowPretty (Fields t) where
    showPrettyIndent xs (Fields ys) = xs ++ intercalate " " (showPretty <$> ys)

instance ShowPretty t => ShowPretty (Constructor t) where
    showPrettyIndent xs (Constructor n fs) = showPrettyIndent xs n ++ " {" ++ showPretty fs ++ "}"

instance ShowPretty t => ShowPretty (Constructors t) where
    showPrettyIndent xs (Constructors cs) =
        xs ++ "  " ++ (intercalate ("\n" ++ xs ++ "| ") $ showPretty <$> Map.elems cs)

instance ShowPretty t => ShowPretty (ADT t) where
    showPrettyIndent xs (ADT n cs) = xs ++ showPretty n ++ " = \n" ++ showPrettyIndent (xs ++ "   ") cs

instance IsString (Constructor t) where
    fromString xs = Constructor (fromString xs) []

pprint :: ShowPretty t => t -> IO ()
pprint = putStrLn . showPretty

instance IsList (Constructors t) where
    type Item (Constructors t) = Constructor t

    fromList xs = Constructors $ Map.fromList $ zip (_constructorName <$> xs) xs
    toList = Map.elems . constructorsToMap

instance IsList (Fields t) where
    type Item (Fields t) = Field t

    fromList = Fields
    toList = fieldsToList

(.:) :: Name -> t -> Field t
(.:) = Field

(.>) :: Name -> [Field t] -> Constructor t
n .> fs = Constructor n (fromList fs)

(.= ) :: Name -> [Constructor t] -> ADT t
n .= cs = ADT n (fromList cs)

csEx0 :: Constructors String
csEx0 =
   [ "Point" .> [ "x" .: "Double", "y" .: "Float"]
   , "Foo"   .> [ "bar" .: "Baz" ]
   ]

mADT :: ADT String
mADT =
    "IntList" .= [ "Nil"
                 , "List" .> ["head" .: "Int", "tail" .: "IntList"
                             ]
                 ]
mADT2 :: ADT String
mADT2 =
    "Foo" .= [ "Bar"   .> [ "baz" .: "Buzz" ]
             , "Hello" .> [ "lens" .: "World"]
             , "This"  .> [ "is" .: "Not", "so" .: "Difficult", "x" .: "Buzz"]
             ]

-- Try this out:
--
-- > pprint csEx0
-- > pprint mADT
--

-- * Our problem.

-- Get all the fields contained on an ADT.
allFields :: ADT t -> [Field t]
allFields adt =
    concatMap fieldsToList $ map _constructorFields $ Map.elems $ constructorsToMap $ _constructors adt
-- Problem:
--
-- It is easy to get lost in the map's, concatMap, conversions from map to lists.
--
-- Could we use lenses somehow to make the definition of fields more understandable?

-- This will get worse once we start with our second problem: convert all the
-- field sort's form @String@ to @Sort@.
--
-- > typeCheckADT :: ADT String -> ADT Sort
--

-- * Looking for a solution using lenses

-- ** Lenses for fields.

fieldName :: Lens' (Field t) Name
--  > Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
--  > h :: (Name -> f Name)
--  > x :: Field t
--  > fieldName h x :: f (Field t)
fieldName h (Field n s) = (`Field` s) <$> h n

-- But you could also use the @lens@ function to create a setter and a getter.
fieldName' :: Lens' (Field t) Name
fieldName' = lens (_fieldName) setter
    where
      setter :: Field t -> Name -> Field t
      setter (Field _ s) n = Field n s

-- sort :: Lens' (Field t) t
sort :: Lens (Field t) (Field s) t s
sort h (Field n s) =  (Field n) <$> h s

-- constructorFields :: Lens' (Constructor t) (Fields t)
constructorFields :: Lens (Constructor t) (Constructor s) (Fields t) (Fields s)
constructorFields h (Constructor n fs) = (Constructor n) <$> h fs

-- Now you see that there's quite a lot of boilerplate involved, so we might as
-- well use makeLenses (from the microlens-th) package.

-- ** Our first traversal

-- fieldsT :: Traversal' (Fields t) [Field t]
fieldsT :: Traversal (Fields t) (Fields s) [Field t] [Field s]
-- fieldsT :: ([Field t] -> f [Field t]) -> Fields t -> f (Fields t)
fieldsT h x =
    Fields <$> h (fieldsToList x)

-- constructorsT :: Traversal' (Constructors t) [Constructor t]
constructorsT :: Traversal (Constructors t) (Constructors s) [Constructor t] [Constructor s]
constructorsT h x = fromList <$> h (toList x)

-- ** Using lenses and traversals to define some functions

allFieldsTmp :: Fields t -> [Field t]
allFieldsTmp fs = fs ^. fieldsT

allFieldsOfConstructorsT :: Traversal' (Constructors t) (Field t)
allFieldsOfConstructorsT = constructorsT
                           . traverse
                           . constructorFields
                           . fieldsT
                           . traverse

allFieldsOfConstructors :: Constructors t -> [Field t]
--
-- > fieldsT                                                           :: Traversal' (Fields t) [Field t]
-- > fieldsT . traverse                                                :: Traversal' (Fields t) (Field t)
-- > constructorFields . fieldsT . traverse                            :: Traversal' (Constructor t) (Field t)
-- > constructorsT . traverse . constructorFields . fieldsT . traverse :: Traversal' (Constructors t) (Field t)
--
allFieldsOfConstructors = toListOf allFieldsOfConstructorsT
-- or equivalently:
--
-- > allFieldsOfConstructors fs = fs ^.. allFieldsOfConstructorsT

-- ** Trying out our traversals!

-- Things to try out:
--
-- >>> csEx0 ^.. allFieldsOfConstructorsT
--
--
-- >>> csEx0 ^.. allFieldsOfConstructorsT . sort
-- ["Baz","Int","Int"]
--
-- >>> over (allFieldsOfConstructorsT . sort) ("SimonSays"<>) csEx0
-- ... this will add "SimonSays" to all the sorts!
--

-- That worked great, so let's try and define the remaining lenses and
-- traversals that we need.

adtName :: Lens' (ADT t) Name
adtName h (ADT n cs) = (`ADT` cs) <$> h n

constructors :: Lens (ADT t) (ADT s) (Constructors t) (Constructors s)
constructors h (ADT n cs) = (ADT n) <$> h cs

-- We can use these new lenses to extract all the sorts of an ADT.
--
-- > mADT2 ^.. (constructors . constructorsT . traverse . constructorFields . fieldsT . traverse . (SimpleADTs.sort))
--
-- But we could also transform all the Strings into Sorts!
--
-- > pprint $ over (constructors . constructorsT . traverse . constructorFields . fieldsT . traverse . (SimpleADTs.sort)) Sort mADT
--
-- And we have the functions we wanted!
typeCheckADT :: ADT String -> ADT Sort
typeCheckADT = over ( constructors
                    . constructorsT
                    . traverse
                    . constructorFields
                    . fieldsT
                    . traverse
                    . sort
                    ) Sort


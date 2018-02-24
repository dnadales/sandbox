-- | 
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedLists   #-}
module SimpleADTs where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map 
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Lens.Micro
import           GHC.Generics (Generic)
import           Data.Hashable
import           GHC.Exts

-- * Our basic data-types.

newtype ADTs = ADTs
    { adtsToMap :: HashMap Name (ADT Sort)
    } deriving (Eq, Show)

data ADT t = ADT
    { adtName :: Name
    , constructors :: Constructors t
    } deriving (Eq, Show)

newtype Name = Name Text deriving (Eq, Show, Generic)

instance Hashable Name

instance IsString Name where
    fromString = Name . T.pack

newtype Constructors t = Constructors
    { constructorsToMap :: HashMap Name (Constructor t)
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

newtype Sort = Sort Text deriving (Eq, Show)

-- * Our problem.

-- Get all the fields contained on an ADT.
allFields :: ADT t -> [Field t]
allFields adt =
    concatMap fieldsToList $ map _constructorFields $ Map.elems $ constructorsToMap $ constructors adt
-- Problem:
--
-- It is easy to get lost in the map's, concatMap, conversions from map to lists.
--
-- Could we use lenses somehow to make the definition of fields more understandable?

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

sort :: Lens' (Field t) t
sort h (Field n s) =  (Field n) <$> h s


constructorFields :: Lens' (Constructor t) (Fields t)
constructorFields h (Constructor n fs) = (Constructor n) <$> h fs

-- Now you see that there's quite a lot of boilerplate involved, so we might as
-- well use makeLenses (from the microlens-th) package.

-- ** Our first traversal

fieldsT :: Traversal' (Fields t) [Field t]
-- fieldsT :: ([Field t] -> f [Field t]) -> Fields t -> f (Fields t) 
fieldsT h x =
    Fields <$> h (fieldsToList x)

instance IsList (Constructors t) where
    type Item (Constructors t) = Constructor t

    fromList xs = Constructors $ Map.fromList $ zip (_constructorName <$> xs) xs
    toList = Map.elems . constructorsToMap

instance IsList (Fields t) where
    type Item (Fields t) = Field t

    fromList = Fields 
    toList = fieldsToList

constructorsT :: Traversal' (Constructors t) [Constructor t]
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

-- First let's define some infix operators to make our life easier:
--
(.:) :: Name -> t -> Field t
(.:) = Field

(.>) :: Name -> [Field t] -> Constructor t
n .> fs = Constructor n (fromList fs)

csEx0 :: Constructors Text
csEx0 =
   [ "Point" .> [ "x" .: "Int", "y" .: "Int"]
   , "Foo"   .> [ "bar" .: "Baz" ]
   ]

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

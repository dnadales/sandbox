{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
-- | Example of typeclasses that can expose a set of typed attributes.

module Introspectable where

-- | Type of the attributes.
data NamedType t where
  AInt :: String -> NamedType Int
  ABool :: String -> NamedType Bool
  AString :: String -> NamedType String

instance Show (NamedType t) where
  show (AInt name) = name ++ ": Int"
  show (ABool name) = name ++ ": Bool"
  show (AString name) = name ++ ": String"
  
data Attribute where
  Attribute :: NamedType t -> Attribute

instance Show Attribute where
  show (Attribute t) = show t

class Introspectable a where
  -- | Get a list of attributes that can be inspected.
  getAttributes :: a -> [Attribute]
  -- | Get an the value for the given attribute.
  getValue :: a -> NamedType t -> Maybe t
  
-- * Some instances.
data Vehicle = Vehicle { vColor :: String
                       , numberOfWheels  :: Int
                       , owner :: String
                       }

data Animal = Animal { sex :: Bool
                     , aColor :: String
                     , numberOfLegs :: Int
                     , speedKmH :: Int
                     }

instance Introspectable Vehicle where
  getAttributes _ = [ Attribute (AString "color")
                    , Attribute (AInt "wheels")
                    ]
  getValue v (AString "color") = Just $ vColor v
  getValue v (AInt "wheels") = Just $ numberOfWheels v
  -- Note that you cannot return a different type:
  --
  -- > getValue v (AInt "wheels") = Just $ "NAN"
  getValue _ _ = Nothing

instance Introspectable Animal where
  getAttributes _ = [ Attribute (AString "color")
                    , Attribute (ABool "sex")
                    ]
  getValue a (AString "color") = Just $ aColor a
  getValue a (ABool "sex") = Just $ sex a
  getValue _ _ = Nothing

-- * Some examples.
v0 = Vehicle "blue" 4 "not me"
v1 = Vehicle "red" 4 "somebody"
a0 = Animal True "black" 4 20
a1 = Animal False "purple" 2 16

-- Try in the REPL:
--
-- > getAttributes v0
-- > getAttributes a1
--
-- etc.
--
-- The retrieve the attributes by using:
--
-- > getValue a0 (ABool "sex")
-- > getValue v1 (AString "color")
--
-- We also get compile time guarantees in the return types:
--
-- > fmap ("dark " ++ ) $ getValue v1 (AString "color")
--
-- will compile (where 'fmap' is the function of the Functor typeclass),
-- however:
--
-- > fmap (10 + ) $ getValue v1 (AString "color")
--
-- will result in a compile time error.
--
-- Problem with this implementation: you cannot use the list returned by
-- 'getAttributes', and you cannot define:
--  
-- > getType :: Attribute -> NamedType t
-- > getType (Attribute nt) = nt
--
-- If you try this you'll get an error like the following:
--
-- >   • Couldn't match type ‘t1’ with ‘t’
-- >      ‘t1’ is a rigid type variable bound by
-- >        a pattern with constructor:
-- >          Attribute :: forall t. NamedType t -> Attribute,
-- >        in an equation for ‘getType’
-- >        at src/Introspectable.hs:24:10
-- >      ‘t’ is a rigid type variable bound by
-- >        the type signature for:
-- >          getType :: forall t. Attribute -> NamedType t
-- >        at src/Introspectable.hs:23:12
-- >      Expected type: NamedType t
-- >        Actual type: NamedType t1
-- >    • In the expression: nt
-- >      In an equation for ‘getType’: getType (Attribute nt) = nt
-- >    • Relevant bindings include
-- >        nt :: NamedType t1 (bound at src/Introspectable.hs:24:20)
-- >        getType :: Attribute -> NamedType t
-- >          (bound at src/Introspectable.hs:24:1)
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE OverloadedLists    #-}
module Lib where

import           Control.Monad.Identity
import           Data.Data
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import qualified Data.Set               (Set)
import           Data.Typeable

data BExpr = Stop
           | Guard Expr BExpr
           | Choice BExpr BExpr
           | Parallel BExpr BExpr
           | Disable BExpr BExpr
           -- ... and so on.
           | Rename (String, String) BExpr -- Substitute a variable name by other
           | Act Expr
           deriving (Eq, Ord, Show, Typeable, Data)

data Expr = Var String | Val Int deriving (Eq, Ord, Show, Data, Typeable)

-- Some examples:
bexp0 = Guard (Var "foo") (Act $ Val 10)
bexp1 = Guard (Var "bar") (Act $ Var "baz")
bexp2 = Choice bexp0 bexp1

subst :: Map String Int -> Expr -> Expr
subst substMap e@(Var name) = maybe e Val $ Map.lookup name substMap
subst _  v@(Val _)          = v

mSubstMap :: Map String Int
mSubstMap = [("foo", 20), ("bar", 30)]

-- > λ> substBExpr mSubstMap bexp2
-- > Choice (Guard (Val 20) (Act (Val 10))) (Guard (Val 30) (Act (Var "baz")))

mSubstMap2 :: Map String Int
mSubstMap2 = [("bar", 30), ("baz", 999)]

-- > λ> substBExpr mSubstMap2 bexp2
-- > Choice (Guard (Var "foo") (Act (Val 10))) (Guard (Val 30) (Act (Val 999)))

substBExpr :: Map String Int -> BExpr -> BExpr
substBExpr _ Stop                     =  Stop
substBExpr substMap (Guard gExp be)   = Guard gExp' be'
    where gExp' = subst substMap gExp
          be' = substBExpr substMap be
substBExpr substMap (Choice be0 be1)  = Choice be0' be1'
    where be0' = substBExpr substMap be0
          be1' = substBExpr substMap be1
substBExpr substMap (Parallel be0 be1) = Parallel be0' be1'
    where be0' = substBExpr substMap be0
          be1' = substBExpr substMap be1
substBExpr substMap (Disable be0 be1) = Disable be0' be1'
    where be0' = substBExpr substMap be0
          be1' = substBExpr substMap be1
-- This might be a tricky case...
-- substBExpr substMap (Rename (from, to) be) = substBExpr substMap (renameBExpr (from, to) be)
substBExpr substMap (Act aExp)        = Act aExp'
    where aExp' = subst substMap aExp

-- rename :: Map String String -> Expr -> Expr
-- rename substMap e@(Var name) = maybe e Var $ Map.lookup name substMap
-- rename _  v@(Val _)          = v


-- One possible solution is to add a type parameter to `BExpr`:
--
-- > data GBExpr e = Stop | Guard e (GBExpr e) | ... deriving Functor
--
-- And then have
--
-- > type BExpr = GBExpr Expr
--
-- > substBExpr substMap = (subst substMap <$>)

data GBExpr e = GStop
              | GGuard e (GBExpr e)
              | GChoice (GBExpr e) (GBExpr e)
              | GParallel (GBExpr e) (GBExpr e)
              | GDisable (GBExpr e) (GBExpr e)
              -- ... and so on.
              | GAct e
              deriving (Show, Functor)

type BExpr2 = GBExpr Expr

substBExpr2 :: Map String Int -> BExpr2 -> BExpr2
substBExpr2 substMap = (subst substMap <$>)

bexp20 = GGuard (Var "foo") (GAct $ Val 10)
bexp21 = GGuard (Var "bar") (GAct $ Var "baz")
bexp22 = GChoice bexp20 bexp21

substT :: (Typeable a) => Map String Int -> a -> a
substT substMap = mkT (subst substMap)

substExprs :: (Data a) => Map String Int -> a -> a
substExprs substMap a = runIdentity $ gfoldl step return (substT substMap a)
    where
      step :: Data d => Identity (d -> b) -> d -> Identity b
      step cdb d = cdb <*> pure (substExprs substMap d)

substExprs2 :: (Data a) => Map String Int -> a -> a
substExprs2 substMap = everywhere (mkT $ subst substMap)

-- > λ> substExprs2 mSubstMap [bexp0, bexp2]
-- > [Guard (Val 20) (Act (Val 10)),Choice (Guard (Val 20) (Act (Val 10))) (Guard (Val 30) (Act (Var "baz")))]

-- substExprs2 mSubstMap [bexp0, bexp2]

mSubstMap3 :: Map String Int
mSubstMap3 = [("foo", 0), ("notAFoo", 0)]
bexp3 = Guard (Var "notAFoo") (Act $ Val 10)

-- Haskell is amazing!
-- > λ> substExprs2 mSubstMap3 $ Set.fromList [bexp0, bexp3]
-- > fromList [Guard (Val 0) (Act (Val 10))]

mSubstMap4 :: Map String Int
mSubstMap4 = [("foo", 0), ("notAFoo", 3)]

-- * Incorporating a rename operation.
bexp4 = Rename ("foo", "bar") bexp0

-- Now we have a problem!
--
-- > λ> substExprs2 mSubstMap $ bexp4
-- > Rename ("foo","bar") (Guard (Val 20) (Act (Val 10)))
--
-- > λ> bexp0
-- > Guard (Var "foo") (Act (Val 10))
--
-- > λ> mSubstMap
-- > fromList [("bar",30),("foo",20)]
--
-- We would like (don't ask me why) that the result is
--
-- > substExprs2 mSubstMap $ Guard (Var "bar") (Act (Val 10))
--
-- Can we define a traversal that accounts for this case while still reducing the boilerplate?

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Substitution that works on `BExpr` and `Expr`
substT1 :: (Typeable a) => Map String Int -> a -> a
substT1 substMap = mkT (subst substMap) `extQ` mkT (rename substMap)

rename :: Map String Int -> BExpr -> BExpr
rename _ (Rename _ _) = Stop -- Just to test the behavior.
rename substMap x     = gmapT (substExprs3 substMap) x -- here we have to continue the
                        -- traversal we use `substExprs4` which maps over the
                        -- immediate sub-terms, avoiding an infinite loop (we
                        -- make sure the map is done on the immediate subterms
                        -- of `x` (instead of `x` itself)).

substExprs3 :: (Data a) => Map String Int -> a -> a
substExprs3 substMap = everywhere (substT1 substMap)

bexp5 = Choice bexp0 bexp4

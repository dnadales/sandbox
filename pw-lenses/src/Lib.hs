{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Playing with lenses.
--
-- Sources:
--
-- - https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references
module Lib where

import           Control.Lens      hiding (Const)
import           Control.Lens.Fold

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | A point in the plane.
data Point = Point { x :: Double, y :: Double } deriving (Show)

data Segment = Segment { from :: Point, to :: Point } deriving (Show)

mkPoint :: (Double, Double) -> Point
mkPoint = uncurry Point

mkSegment :: (Double, Double) -> (Double, Double) -> Segment
mkSegment fromXY toXY = Segment (mkPoint fromXY) (mkPoint toXY)

-- | Increase the x coordinate of `from`:
shiftFromX :: Segment -> Segment
shiftFromX seg = seg { Lib.from = newFrom }
  where newFrom = (Lib.from seg) { x = newX }
          where newX = x (Lib.from seg) + 1

-- | Alternatively:
shiftFromX' :: Segment -> Segment
shiftFromX' (Segment (Point x y) to) = Segment (Point (x + 1) y) to

testSeg :: Segment
testSeg = mkSegment (0, 1) (2, 3)

doubleEndY :: Segment -> Segment
doubleEndY (Segment from (Point x y)) = Segment from (Point x (y * 2))

-- | Getting fields is easy. For instance, if we want the `y` coordinate of the
-- end-point of the segment we can do the following:
--
-- > y . to $ testSeg
--
-- But what if we wanted to update the point above?
--
-- > testSeg { to = mkPoint (2, 44)}
--
-- And it gets even worse when trying to reach a nested field: let's double the
-- field above:
--
-- > let end = to testSeg in testSeg { to = end { y = 2 * y end }}
--
-- Nasty, nasty ...

-- * A lensed point.
data PointL = PointL { _posX :: Double, _posY :: Double } deriving (Show)
makeLenses ''PointL

-- makeLenses will generate:
--
-- > *Main Lib> :i posX
-- > posX :: Control.Lens.Type.Lens' PointL Double
--
-- > *Main Lib> :i posY
-- > posY :: Control.Lens.Type.Lens' PointL Double
--
-- > *Main Lib> :i start
-- > start :: Control.Lens.Type.Lens' SegmentL PointL
--
-- > *Main Lib> :i end
-- > end :: Control.Lens.Type.Lens' SegmentL PointL
--

data SegmentL = SegmentL { _start :: PointL, _end :: PointL } deriving (Show)
makeLenses ''SegmentL

mkPointL :: (Double, Double) -> PointL
mkPointL = uncurry PointL

mkSegmentL :: (Double, Double) -> (Double, Double) -> SegmentL
mkSegmentL startXY endYX = SegmentL (mkPointL startXY) (mkPointL endYX)

-- * Using the lenses combinators
--
-- `view` works just as a record accessor:
--
-- > let testSeg = mkSegmentL (0, 1) (2, 4)
-- > view end testSeg
--
-- `set` is a record setter:
--
-- > set end (mkPointL (2, 3)) testSeg
--
-- `view` compose:
--
-- > view (start . posY) testSeg
-- > view (end . posY) testSeg

-- ** Getting out of the mess
--
-- `over` allows to apply an arbitrary function to a nested field, thus helping
-- us having more readable code:
--
-- > over (end . posY) (*2) testSeg

-- * The key question:
--
-- > How come composing lenses with (.) just works?

shiftFromXL :: SegmentL -> SegmentL
shiftFromXL seg = over (start . posX) (+1) seg

testSegL :: SegmentL
testSegL = mkSegmentL (0, 1) (2, 4)

-- * Making sense out of lenses
--

-- ** Traversals
pointCoordinates :: Applicative f
                 => (Double -> f Double) -> Point -> f Point
pointCoordinates f (Point x y) = Point <$> f x <*> f y

-- We can use `pointCoordinates` to implement `rejectWithNegatives`.
deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
deleteIfNegative x = if x < 0 then Nothing else Just x

rejectWithNegatives :: Point -> Maybe Point
rejectWithNegatives p = pointCoordinates deleteIfNegative p

-- > rejectWithNegatives (mkPoint (1, 2))
-- > rejectWithNegatives (mkPoint (-1, 2))

-- | The `MTraversal` type:
--
-- Function `pointCoordinates` is an example of a more generalized notion:
-- `Traversal`, here called `MTraversal` to avoid a name clash with the one
-- defined at `Lens`.
--
type MTraversal s t a b =
  forall f . Applicative f => (a -> f b) -> s -> f t
--
-- compare this with the type of `traverse`:
--
-- > traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
--
-- Note that:
--
-- - `f` is not mentioned on the left side (it is universally quantified).
-- - `t a` in traverse is replaced by `s`, and `t b` is replaced by `t`.
pointCoordinatesT :: MTraversal Point Point Double Double
pointCoordinatesT f (Point x y) = Point <$> f x <*> f y

-- *** Exercises

extremityCoordinates :: MTraversal Segment Segment Double Double
extremityCoordinates f (Segment fr to) =
  Segment <$> pointCoordinatesT f fr <*> pointCoordinatesT f to

-- ** Setters

-- | The `Setter` type, here called `MSetter` to avoid a name clash with the
-- lens one.
--
type MSetter s t a b =
  (a -> Identity b) -> s -> Identity t

-- | Modify the target of a lens.
--
-- The original type of `over` is:
--
-- > over :: ASetter s t a b -> (a -> b) -> s -> t
mOver :: MSetter s t a b -> (a -> b) -> s -> t
mOver l f s = runIdentity $ l (Identity . f) s

-- *** Exercises

-- | Multiply all the coordinates of a segment by the given constant.
--
-- Examples:
--
-- >>> s0 = Segment (Point 0 1) (Point 3 4)
-- >>> scaleSegment 10 s0
-- Segment {from = Point {x = 0.0, y = 10.0}, to = Point {x = 30.0, y = 40.0}}
--
scaleSegment :: Double -> Segment -> Segment
scaleSegment c = mOver extremityCoordinates (*c)

-- | Apply a function over all values of a functor.
mMapped :: Functor f => MSetter (f a) (f b) a b
mMapped g fa = Identity $ (runIdentity . g) <$> fa

-- ** Folds

-- | The fold type.
type MFold s a =
  forall f . (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

-- | Predicate is an example of a contravariant instance:
--
-- >>> let largerThanFour = Predicate (> 4)
-- >>> getPredicate largerThanFour 6
-- True
-- >>> getPredicate (contramap length largerThanFour) "orange"
-- True
--
newtype Predicate a = Predicate { getPredicate :: a -> Bool }

instance Contravariant Predicate where
  -- contramap :: (a -> b) -> Predicate b -> Predicate a
  contramap f (Predicate p) = Predicate $ p . f

-- | We can use lenses functions with our ad-hoc lenses!
toListListsExample =
  toListOf extremityCoordinates (Segment (Point 0 1) (Point 3 4))
previewExample = preview traverse [1..10]

-- ** Getters

-- * Lenses at last

-- TODO: Continue here: https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references#Lenses_at_last


-- Some test

-- Lenses for Sum types with partial record accessors.

data Const
    = -- | Constructor of Boolean constant.
      Cbool    { _toBool :: Bool }
    --   -- | Constructor of Integer constant.
    | Cint     { _toInteger :: Integer }
    deriving (Eq, Ord, Read, Show)

makeLenses ''Const

mBool :: Lens' Const (Maybe Bool)
mBool f (Cbool b) = (\mb ->
    case mb of
        Nothing -> Cbool False
        Just b' -> Cbool b' ) <$> f (Just b)
mBool f v = const v <$> f Nothing

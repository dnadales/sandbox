{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Playing with lenses.
--
-- Sources:
--
-- - https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references
module Lib
    ( someFunc
    , Point (..)
    , Segment (..)
    , mkPoint
    , mkSegment
    , PointL (..)
    , posX, posY
    , SegmentL (..)
    , start, end
    , mkPointL
    , mkSegmentL
    ) where

import           Control.Lens

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | A point in the plane.
data Point = Point { x :: Double, y :: Double } deriving (Show)

data Segment = Segment { from :: Point, to :: Point } deriving (Show)

mkPoint :: (Double, Double) -> Point
mkPoint = uncurry Point

mkSegment :: (Double, Double) -> (Double, Double) -> Segment
mkSegment fromXY toXY = Segment (mkPoint fromXY) (mkPoint toXY)

-- | Getting fields is easy. For instance, if we want the `y` coordinate of the
-- end-point of the segment we can do the following:
--
-- > let testSeg = mkSegment (0, 1) (2, 3)
-- > y . to $ testSeg
-- >
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

-- * Making sense out of lenses
--

module Glossify where

import Control.Lens

import qualified Graphics.Gloss as G
import qualified Data.CircularSeq as C
import Data.Foldable
import Data.Geometry hiding (init)
import Data.Ext

import Data.Geometry.Point
import Data.Geometry.Polygon
import Data.Geometry.LineSegment
import Data.Geometry.BezierSpline
import Data.Geometry.PlanarSubdivision

import Algorithms.Geometry.PolygonTriangulation.Triangulate

import           GHC.TypeNats


class Glossifiable a b where
  glossify :: a -> b

instance Real r => Glossifiable (Point 2 r) G.Point where
  glossify = glossifyPoint

instance RealFrac r => Glossifiable (Polygon t p r) G.Picture where
  glossify = glossifyPolygon

instance Real r => Glossifiable (LineSegment 2 p r) G.Picture where
  glossify = glossifySegment

instance (Glossifiable a c, Glossifiable b c) => Glossifiable (Either a b) c where
  glossify (Left  l) = glossify l
  glossify (Right r) = glossify r


instance (RealFrac r, Show r) => Glossifiable (BezierSpline 3 2 r) G.Picture where
  glossify = glossifyBezier 10 1



glossifyPoint :: Real r => Point 2 r -> G.Point
glossifyPoint (Point2 x y) = (realToFrac x, realToFrac y)

glossifyPolygon :: (RealFrac) r => Polygon t p r -> G.Picture
glossifyPolygon p = 
  let t  = triangulate undefined p
      fs = toList $ faces' t
      is = filter (\f -> t ^. dataOf f == Inside) fs
      ts = map _core $ map (flip faceBoundary t) is
  in G.Pictures $ map glossifyConvexPolygon ts

glossifyConvexPolygon :: RealFrac r => Polygon t p r -> G.Picture
glossifyConvexPolygon p | isSimple p = G.Polygon $ map (glossify . view core) $ toPoints p
                        | otherwise  = G.Pictures $ glossify (view outerBoundary p)
                                                  : map (G.Color G.white . glossify) (holeList p)

glossifyCSeq :: Real r => C.CSeq (Point 2 r :+ p) -> [G.Point]
glossifyCSeq = map glossifyPoint . map _core . toList

glossifySegment :: Real r => LineSegment 2 p r -> G.Picture
glossifySegment s = let (p :+ _, q :+ _) = orderedEndPoints s 
                    in G.Line $ map glossifyPoint [p, q]

glossifyBezier :: (RealFrac r, Show r) => r -> r -> BezierSpline 3 2 r -> G.Picture
glossifyBezier resolution width b = thickLine (realToFrac width) $ map glossifyPoint $ map _core $ toList $ _points $ approximate resolution b








thickLine :: Float -> G.Path -> G.Picture
thickLine w ps = G.Pictures $ map v ps ++ map e (zip (init ps) (tail ps))
  where
    v :: G.Point -> G.Picture
    v (x, y) = G.translate x y $ G.circleSolid $ w/2
    e :: (G.Point, G.Point) -> G.Picture
    e ((px, py), (qx, qy)) = 
      let l  = sqrt $ (qx - px) ** 2 + (qy - py) ** 2
          dx = (qx - px) / l 
          dy = (qy - py) / l 
      in G.Polygon [ (px + w/2 * dy, py - w/2 * dx)
                   , (px - w/2 * dy, py + w/2 * dx)
                   , (qx - w/2 * dy, qy + w/2 * dx)
                   , (qx + w/2 * dy, qy - w/2 * dx)
                   ]


module Graphics.Geometry.Rasterific where

-- Instances of hgeometry types of Rasterific's 'Geometry' class.


import qualified Graphics.Rasterific as R

import Data.Geometry
import Data.Geometry.Point
import Data.Geometry.Polygon
import Data.Geometry.LineSegment
import Data.Geometry.PlanarSubdivision
import Data.Geometry.BezierSpline

import Data.Ext
import Data.Foldable
import qualified Data.CircularSeq as C
import qualified Data.LSeq as L

import Control.Lens
import           GHC.TypeNats



-- maybe need to define a new class after all, to avoid overlapping instances
-- check if existing instances "do the right thing"?

class RGeometry a where
  toPrimitives :: a -> [R.Primitive]

instance Real r => RGeometry (LineSegment 2 p r) where
  toPrimitives = rasterifySegment

instance RealFrac r => RGeometry (Polygon t p r) where
  toPrimitives = rasterifyPolygon

instance RealFrac r => RGeometry (PolyLine 2 p r) where
  toPrimitives = rasterifyPolyLine
 
 {-
instance (R.Geometry a, R.Geometry b) => R.Geometry (Either a b) where
  toPrimitives (Left  l) = R.toPrimitives l
  toPrimitives (Right r) = R.toPrimitives r
-}

instance RealFrac r => RGeometry (BezierSpline 2 2 r) where
  toPrimitives = rasterifyQuadraticBezier

instance RealFrac r => RGeometry (BezierSpline 3 2 r) where
  toPrimitives = rasterifyCubicBezier





rasterifyQuadraticBezier :: RealFrac r => BezierSpline 2 2 r -> [R.Primitive]
rasterifyQuadraticBezier (Bezier2 p q r) = [R.BezierPrim $ R.Bezier (ras p) (ras q) (ras r)]

rasterifyCubicBezier :: RealFrac r => BezierSpline 3 2 r -> [R.Primitive]
rasterifyCubicBezier (Bezier3 p q r s) = [R.CubicBezierPrim $ R.CubicBezier (ras p) (ras q) (ras r) (ras s)]

rasterifyAnyBezier :: (KnownNat n, RealFrac r, Show r) => r -> BezierSpline n 2 r -> [R.Primitive]
rasterifyAnyBezier r b = R.polyline $ map ras $ map _core $ toList $ _points $ approximate r b








rasterifyPoint :: Real r => Point 2 r -> R.Point
rasterifyPoint (Point2 x y) = R.V2 (realToFrac x) (realToFrac y)

ras :: Real r => Point 2 r -> R.Point
ras = rasterifyPoint

rasterifyPolygon :: (RealFrac) r => Polygon t p r -> [R.Primitive]
rasterifyPolygon (MultiPolygon p ps) = concatMap rasterifyPolygon $ p:ps
rasterifyPolygon p                   = R.polygon $ map (ras . _core) $ toPoints p

rasterifyPolyLine :: (RealFrac) r => PolyLine 2 p r -> [R.Primitive]
rasterifyPolyLine (PolyLine s) = R.polyline $ rasterifyLSeq s
  
rasterifyCSeq :: Real r => C.CSeq (Point 2 r :+ p) -> [R.Point]
rasterifyCSeq = map ras . map _core . toList

rasterifyLSeq :: Real r => L.LSeq 2 (Point 2 r :+ p) -> [R.Point]
rasterifyLSeq = map ras . map _core . toList

rasterifySegment :: Real r => LineSegment 2 p r -> [R.Primitive]
rasterifySegment s = R.line (ras $ s ^. start ^. core) (ras $ s ^. end ^. core)


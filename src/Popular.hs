module Popular where


import Control.Lens
import Data.Ext
import Data.Foldable (toList)

import Data.Geometry.Polygon
import Data.Geometry.PlanarSubdivision hiding (endPoints)
import Data.Geometry.PlanarSubdivision.More
import Data.Geometry.BezierSpline

import Graphics.Gloss hiding (Point, Vector, Arc, Path, arc, display)

import PSDGlossApp
import PSDGlossApp.Visible

import CurveArrangement.Types
import CurveArrangement.Basic

import Nonogram
import Nonogram.PathType

import Graphics.Geometry.Gloss

import PP.Puz

import Debug.Trace

---------------------------
-- finding popular faces --
---------------------------

markPopularFaces :: CA r -> CA r
markPopularFaces ca = foldr markPopularFacesFrom (erasePopularFaces ca) $ entryDarts ca

markPopularFacesFrom :: Dart CAS -> CA r -> CA r
markPopularFacesFrom d ca = let curve = traceCurve ca d
                                faces = map fst $ filter snd $ markDuplicates $ removeConsecutiveDuplicates $ leftFaces ca curve
                            in foldr (\i c -> c & dataOf i . popular .~ True) ca faces

erasePopularFaces :: CA r -> CA r
erasePopularFaces ca = foldr (\i c -> c & dataOf i . popular .~ False) ca $ faces' ca

markDuplicates :: Eq a => [a] -> [(a, Bool)]
markDuplicates xs = map (\x -> (x, count x xs >= 2)) xs

count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (== x) xs

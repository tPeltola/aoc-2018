module Six where

import Data.List.Split
import Data.List
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Sequence (Seq, (><))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

type Point = (Int, Int)
data Area = Draw | Area { areaSource :: Source } deriving (Show, Eq)
data Source = Source 
    { distanceFrom :: Int
    , fromPoint :: Point
    } deriving (Show, Eq)

toCoordinates :: String -> Point
toCoordinates line = case map read $ splitOn "," line of
                        (x:y:[]) -> (x, y)

distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

solve1 input = maximum $ map areaSize $ filter (flip Set.notMember infinite) sources
    where 
        infinite = Set.fromList ((toList (((headAndLast points) >>= id) >< edges)) >>= sourcePoint)
        sourcePoint Draw = []
        sourcePoint (Area (Source _ p)) = [p]
        edges = points >>= headAndLast
        headAndLast s = Seq.fromList [Seq.index s 0, Seq.index s (Seq.length s - 1)]
        points = Seq.fromFunction (4 * maxX) withX
        sources = map toCoordinates input
        maxX = maximum $ map fst sources
        maxY = maximum $ map snd sources
        areaSize s = sum $ fmap (Seq.length . Seq.filter (inArea s)) points
        inArea s Draw = False
        inArea s (Area (Source _ p)) = s == p
        withX x = Seq.fromFunction (4 * maxY) (withXy x)
        withXy x' y' = let x = x' - 2 * maxX
                           y = y' - 2 * maxY
                           asArea source = (distanceFrom source, Area source)
                           distances = Map.fromListWith (const . const Draw) $ map asArea $ zipWith Source (map (distance (x, y)) sources) sources
                        in snd $ Map.findMin distances

solve2 input = regionSize
    where 
        points = Seq.fromFunction (4 * maxX) withX
        sources = map toCoordinates input
        maxX = maximum $ map fst sources
        maxY = maximum $ map snd sources
        regionSize = sum $ fmap (Seq.length . Seq.filter (< 10000)) points
        withX x = Seq.fromFunction (4 * maxY) (withXy x)
        withXy x' y' = let x = x' - 2 * maxX
                           y = y' - 2 * maxY
                        in sum $ map (distance (x, y)) sources

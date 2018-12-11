module Ten where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Number

import Data.List
import qualified Data.Set as Set

type XY = (Int, Int)

starParser = (,)
    <$> (string "position=" *> xyParser)
    <*> (string " velocity=" *> xyParser)
    where xyParser = (,)
            <$> (char '<' *> spaces *> int)
            <*> (char ',' *> spaces *> int <* char '>')

parseStar :: String -> (XY, XY)
parseStar line = either (error . show) id $ parse starParser "" line

spellOut stars = unlines $ map row [minY..maxY]
    where
        positions = map fst stars
        minX = minimum $ map fst positions
        maxX = maximum $ map fst positions
        minY = minimum $ map snd positions
        maxY = maximum $ map snd positions
        row y = map (cell y) [minX..maxX]
        cell y x = if elem (x, y) positions then '#' else '.'

move stars = map go stars
    where go ((x,y), (vx, vy)) = ((x + vx, y + vy), (vx, vy))

allAdjacent stars = all hasAdjacent stars
    where 
        positions = Set.fromList $ map fst stars
        hasAdjacent ((x, y), _) = any (flip Set.member positions) [(x + i, y + j) | i <- [-1, 0, 1], j <- [-1, 0, 1], i /= 0 || j /= 0 ]

solve1 input = spellOut $ head $ filter allAdjacent $ iterate move starMap 
    where starMap = map parseStar input
solve2 input = length $ takeWhile (not . allAdjacent) $ iterate move starMap 
    where starMap = map parseStar input

module Three where

import Text.Parsec
import Text.Parsec.Char

import Data.Either
import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Claim = Claim 
    { claimId :: Int
    , x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    } deriving Show

invalidClaim = Claim (-1) 0 0 0 0

claimParser = Claim 
        <$> (hash *> num) 
        <*> (space *> at *> space *> num) 
        <*> (comma *> num) 
        <*> (colon *> space *> num) 
        <*> (x *> num) 
    where 
        hash = char '#'
        at = char '@'
        comma = char ','
        colon = char ':'
        x = char 'x'
        num :: Parsec String st Int
        num = read <$> many1 digit

parseClaims lines = map parseClaim lines
    where parseClaim line = fromRight invalidClaim $ parse claimParser "" line

makeClaim fabric claim = Map.unionWith (++) claimed fabric
    where 
        claimed = Map.fromList [((i, j), [claimId claim]) | i <- [x0..x1], j <- [y0..y1]]
        x0 = x claim
        x1 = (x claim) + (width claim) - 1
        y0 = y claim
        y1 = (y claim) + (height claim) - 1

solve1 :: [String] -> Int
solve1 input = length $ filter ((> 1) . length) $ Map.elems claimed
    where claimed = foldl makeClaim Map.empty $ parseClaims input


solve2 :: [String] -> Int
solve2 input = head $ Set.toList $ Set.difference claimIds overlapping
    where
        claims = parseClaims input
        claimed = foldl makeClaim Map.empty claims
        claimIds = Set.fromList $ map claimId claims
        overlapping = Set.fromList $ concat $ filter ((> 1) . length) $ Map.elems claimed
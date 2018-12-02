module Two where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

readInput = do 
    content <- readFile "input/02.txt"
    pure $ lines content

solve1 :: IO Int
solve1 = do
    ids <- readInput
    pure $ checksum ids

checksum ids = countOfThree * countOfTwo
    where 
        countOfThree = length $ filter (hasExactly 3) counts
        countOfTwo = length $ filter (hasExactly 2) counts
        counts = letterCounts ids
        hasExactly x counts = elem x $ Map.elems counts 

letterCounts ids = map count ids
    where count id = Map.fromListWith (+) $ zip id $ repeat 1 

solve2 :: IO String
solve2 = do
    ids <- readInput
    pure $ matches ids

matches ids = head $ filter ((== 25) . length) $ map matching candidates
    where
        candidates = candidates' ids []
        candidates' [] acc = acc
        candidates' (first:rest) acc = candidates' rest (acc ++ (zip rest $ repeat first))
        matching (l, r) = matching' l r []
        matching' [] [] acc = reverse acc
        matching' (x:xs) (y:ys) acc
            | x == y = matching' xs ys (x:acc)
            | otherwise = matching' xs ys acc
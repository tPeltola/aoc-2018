module One where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

readInput = do 
    content <- readFile "input/01.txt"
    pure $ lines content

toFrequencyChange str = sign * num
    where
        sign = case first of
            '+' -> 1
            '-' -> -1
        num = read rest
        (first:rest) = str

solve1 :: IO Int
solve1 = do
    input <- readInput
    let changes = map toFrequencyChange input
    let final = 0 + (sum changes)
    pure final

solve2 :: IO Int
solve2 = do
    input <- readInput
    let changes = map toFrequencyChange input
    let repeating = cycle changes
    let frequencies = scanl (+) 0 repeating
    pure $ firstDuplicate frequencies

firstDuplicate list = go list Set.empty
    where
        go (first:rest) seen
            | Set.member first seen = first
            | otherwise = go rest $ Set.insert first seen
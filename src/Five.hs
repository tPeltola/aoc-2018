module Five where

import Data.Char
import Data.List
import Data.Maybe

react polymer = reverse $ go polymer []
    where 
        go [] acc = acc
        go (x:[]) acc = x:acc
        go (x:y:rest) acc 
            | isUpper x && toLower x == y = go rest' acc'
            | isLower x && toUpper x == y = go rest' acc'
            | otherwise = go (y:rest) (x:acc)
                where (rest', acc') = case acc of
                        []     -> (rest, acc)
                        (c:cs) -> (c:rest, cs)
                        
remove polymer pair = reverse $ foldl go [] polymer
    where go acc x
            | pair == x = acc
            | toUpper pair == x = acc
            | otherwise = x:acc

solve1 input = length $ react $ head input
solve2 input = minimum $map length $ map react $ map (remove $ head input) ['a'..'z']
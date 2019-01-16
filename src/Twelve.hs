module Twelve where

import Text.Parsec
import Text.Parsec.Char

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

generationParser :: Parsec String st (Map String Char)
generationParser = Map.fromList <$> many1 line
    where 
        line = (,) <$> (llcr <* string " => ") <*> (n <* ((const () <$> endOfLine) <|> eof))
        llcr = many1 $ oneOf ".#"
        n = oneOf ".#"

initialParser :: Parsec String st String
initialParser = string "initial state: " *> (many1 $ oneOf ".#") <* (endOfLine *> endOfLine)

parser = (,) <$> initialParser <*> generationParser

parse = do
    input <- readFile "input/12.txt"
    pure $ either (error . show) id $ runParser parser () "" input

grow n initial generations = zip [-n..] $ foldl go initial' [1..n]
    where
        initial' = (take n $ repeat '.') ++ initial ++ (take n $ repeat '.')
        go plants _ = map nextGen $ zipWith5 conc (ee ++ plants) (e ++ plants) plants (drop 1 $ plants ++ e) (drop 2 plants ++ ee)
        nextGen llcrr = Map.findWithDefault '.' llcrr generations
        conc a b c d e = [a,b,c,d,e]
        ee = ".."
        e = "."

solve1 (initial, generations) = sum $ map fst $ filter ((== '#') . snd) $ grow 20 initial generations
solve2 (initial, generations) = generations

module Nine where

import Text.Parsec
import Text.Parsec.Char

import Data.List
import Data.Sequence (Seq)
import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Game = Game
    { numOfPlayers :: Int
    , lastMarble :: Int
    } deriving Show

gameParser = Game
    <$> (num <* string " players;")
    <*> (string " last marble is worth " *> num <* string " points")
    where num = read <$> many1 digit

parseGame :: String -> Game
parseGame line = either (error . show) id $ parse gameParser "" line


turn :: Game -> (Seq Int, Int, Map Int (Set Int)) -> Int -> (Seq Int, Int, Map Int (Set Int)) 
turn game (marbles, current, players) marble
    | marble `mod` 23 == 0 = (marbles'', current'', scored)
    | otherwise = (marbles', current', players)
    where
        insertTo = (current + 2) `mod` Seq.length marbles
        marbles' = Seq.insertAt insertTo marble marbles
        current' = insertTo
        deleteFrom = (Seq.length marbles + current - 7) `mod` Seq.length marbles
        scored = Map.insertWith Set.union ((marble - 1) `mod` numOfPlayers game + 1) (Set.fromList [marble, Seq.index marbles deleteFrom]) players
        marbles'' = Seq.deleteAt deleteFrom marbles 
        current'' = deleteFrom

play game = foldl (turn game) (Seq.singleton 0, 0, Map.empty) [1..(lastMarble game)]

trd (_, _, x) = x

solve1 input = maximum $ map sum $ Map.elems $ trd $ play game
    where game = parseGame $ head input
    
solve2 input = maximum $ map sum $ Map.elems $ trd $ play $ Game (numOfPlayers game) (100 * lastMarble game)
    where game = parseGame $ head input
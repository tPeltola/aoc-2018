module Four where

import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import Data.Either
import Data.Time
import Data.Time.Format

import Text.Parsec
import Text.Parsec.Char

data Log = Log
    { time :: UTCTime
    , event :: Event
    } deriving Show

data Event = WakeUp | FallAsleep | BeginShift { guardId :: Int } deriving Show

invalidLog = error "failed to parse"

logParser = Log <$> timeParser <*> eventParser
    where
        timeParser = parseTime <$> ((char '[') *> (many1 $ noneOf "]") <*  (char ']') <* space)
        parseTime :: String -> UTCTime
        parseTime time = fromJust $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d %H:%M" time 
        eventParser = wakeUp <|> fallAsleep <|> beginShift
        wakeUp = (const WakeUp) <$> string "wakes up"
        fallAsleep = (const FallAsleep) <$> string "falls asleep"
        beginShift = BeginShift <$> ((string "Guard #") *> num <* (string " begins shift"))
        num :: Parsec String st Int
        num = read <$> many1 digit


parseLogs lines = map parseLog lines
    where parseLog line = either (error . show) id $ parse logParser "" line 
    
sleepingOnDuty input = Map.toList $ trd $ foldl go (Nothing, Nothing, Map.empty) $ sortOn time $ parseLogs input
    where 
        go (_, _, acc) (Log time (BeginShift guard)) = (Just guard, Nothing, acc)
        go (guard, _, acc) (Log time FallAsleep) = (guard, Just time, acc)
        go (Just guard, Just fellAsleep, acc) (Log time WakeUp) = (Just guard, Nothing, Map.insertWith (++) guard (asleep fellAsleep time) acc)
        trd (_, _, x) = x
        asleep from to = [(round $ utctDayTime from) `div` 60 .. (round $ utctDayTime to) `div` 60 - 1]

solve1 input = times $ head $ reverse $ sortOn (length . snd) $ sleepingOnDuty input
    where times (id, slept) = id * (fst $ head $ reverse $ sortOn snd $ Map.toList $ Map.fromListWith (+) $ zip slept $ repeat 1)

solve2 input = times $ head $ reverse $ sortOn (snd . snd) $ map asleepMostAt $ sleepingOnDuty input
    where 
        asleepMostAt (id, slept) = (id, (head $ reverse $ sortOn snd $ Map.toList $ Map.fromListWith (+) $ zip slept $ repeat 1))
        times (id, (minute, _)) = id * minute

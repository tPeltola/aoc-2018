module Seven where

import Text.Parsec
import Text.Parsec.Char

import qualified Data.Set as Set
import Data.Graph
import Data.Char
import Data.List

data Dependency = Dependency
    { to :: Char
    , from :: Char
    } deriving Show

dependencyParser = Dependency 
    <$> (string "Step " *> anyChar)
    <*> (string " must be finished before step " *> anyChar <* string " can begin.")

parseDependency :: String -> Dependency
parseDependency line = either (error . show) id $ parse dependencyParser "" line

order dependencies = go steps []
    where
        steps = Set.fromList ((map from dependencies) ++ (map to dependencies))
        go incomplete completed
            | Set.null incomplete = reverse completed
            | otherwise = go (Set.delete next incomplete) (next:completed)
            where 
                next = Set.findMin satisfied
                satisfied = Set.filter isSatisfied incomplete
                isSatisfied step = all (flip elem completed . to) $ filter ((== step) . from) dependencies
              

solve1 input = order $ map parseDependency input
solve2 input = run $ map parseDependency input

run dependencies = go steps [] [] 0
    where
        steps = Set.fromList ((map from dependencies) ++ (map to dependencies))
        go incomplete inProgress completed time
            | Set.null incomplete && null inProgress = time - 1
            | otherwise = go incomplete' inProgress' completed' (time + 1)
            where 
                incomplete' = Set.difference incomplete $ Set.fromList $ map fst started
                inProgress' = stillInProgress ++ started
                (justCompleted, stillInProgress) = partition hasCompleted inProgress
                completed' = completed ++ map fst justCompleted
                hasCompleted (step, started) = timeToComplete step <= time - started
                available = workers - length stillInProgress
                started = map withStart $ Set.toList $ Set.take available satisfied
                withStart step = (step, time)
                satisfied = Set.filter isSatisfied incomplete
                isSatisfied step = all (flip elem completed' . to) $ filter ((== step) . from) dependencies

workers = 5
timeToComplete step = ord step - ord 'A' + 61
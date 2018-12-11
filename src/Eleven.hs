module Eleven where

import qualified Data.Sequence as Seq
import Data.List
import Data.Ord

serialNumber = 3214

powerLevel (x, y) = hundreds ((rackId * y + serialNumber) * rackId) - 5
    where
        rackId = x + 10
        hundreds n = n `div` 100 `mod` 10

cells = Seq.fromFunction 300 withX
    where 
        withX x = Seq.fromFunction 300 (withXy x)
        withXy x y = powerLevel (x + 1, y + 1)

squarePower (size, (x, y)) = sum $ map cellPower [(x + i, y + j) | i <- [-1..size-2], j <- [-1..size-2]]
    where cellPower (x, y) = Seq.index (Seq.index cells x) y

solve1 input = maximumBy (comparing squarePower) [(3, (x, y)) | x <- [1..300], y <- [1..300], x <= 298, y <= 298]
solve2 input = maximumBy (comparing squarePower) [(size, (x, y)) | x <- [1..300], y <- [1..300], size <- [1..300], x + size <= 301, y + size <= 301]
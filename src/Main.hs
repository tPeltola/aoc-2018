module Main where

import Twelve

readInput = do 
    content <- readFile "input/10.txt"
    pure $ lines content

main :: IO ()
main = do
    --input <- readInput
    input <- parse
    let solution1 = solve1 input
    putStrLn $ show solution1
    let solution2 = solve2 input
    putStrLn $ show solution2

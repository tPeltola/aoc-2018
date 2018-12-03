module Main where

import Three

readInput = do 
    content <- readFile "input/03.txt"
    pure $ lines content

main :: IO ()
main = do
    input <- readInput
    let solution1 = solve1 input
    putStrLn $ show solution1
    let solution2 = solve2 input
    putStrLn $ show solution2

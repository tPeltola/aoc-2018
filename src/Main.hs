module Main where

import One

main :: IO ()
main = do
    solution1 <- solve1
    putStrLn $ show solution1
    solution2 <- solve2
    putStrLn $ show solution2

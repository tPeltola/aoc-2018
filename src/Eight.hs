module Eight where

import Data.List
import Data.List.Split
import qualified Data.Graph as G

parseTree = fst . parseNode
    where
        parseNode (nodeCount:metadataCount:afterHeader) = (G.Node metadata (reverse children), afterMetadata)
            where 
                (children, afterChildren) = foldl parseChild ([], afterHeader) $ take nodeCount $ repeat ()
                parseChild (acc, d) _ = let (child, d') = parseNode d 
                                            acc' = child:acc
                                         in (acc', d')
                (metadata, afterMetadata) = splitAt metadataCount afterChildren

toTree line = parseTree $ map read $ splitOn " " line

solve1 input = sum $ fmap sum $ toTree $ head input
solve2 input = value $ toTree $ head input

value (G.Node meta []) = sum meta 
value (G.Node meta children) = sum $ map childValue meta
    where 
        childValue i 
            | i > 0 && i <= length children = value (children !! (i - 1))
            | otherwise = 0  
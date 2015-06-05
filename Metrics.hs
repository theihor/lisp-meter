module Metrics 
( countOfSexprs
, countOfLines
, countOfLists
, countOfFunctions
, averageFunctionSize
, halsteadMetrics
) where

import Parser
import Utils
import qualified Data.Map as Map

-- size metrics
countOfLines :: String -> Int
countOfLines input = length $ lines input

countOfSexprs :: [Node] -> Integer
countOfSexprs term = foreachCount term ["_"]

countOfLists :: [Node] -> Integer
countOfLists term = foreachCount term ["(_*)"]

countOfFunctions :: [Node] -> Integer
countOfFunctions term = foreachCount term ["(defun _*)"]

averageFunctionSize :: [Node] -> Double
averageFunctionSize term =
    let funcs = foreachCollect term ["(defun _*)", "(defmethod _*)"] 
        sizes = map (\f -> countOfSexprs [f]) funcs 
        s = foldl (+) 0 sizes 
    in (fromIntegral s) / (fromIntegral $ length funcs)

-- Halstead
halsteadOperators :: [Node] -> (Int, Int)
halsteadOperators term =
    let allLists = foreachCollect term ["(_*)"]
        allOperators = filter nodeSymbol $ map (\n -> head $ nodeComponents n) allLists 
        uniqueOperators = removeDuplicates allOperators
    in (length allOperators, length uniqueOperators)

halsteadOperands :: [Node] -> (Int, Int)
halsteadOperands term =
    let allLists = foreachCollect term ["(_*)"]
        allOperands = foldl (++) [] $ map (\n -> tail $ nodeComponents n) allLists
        uniqueOperands = removeDuplicates allOperands
    in (length allOperands, length uniqueOperands)

halsteadMetrics :: [Node] -> Map.Map String Double 
halsteadMetrics term =
    let (n1, n1') = (\(x, y) -> (fromIntegral x, fromIntegral y)) 
                    $ halsteadOperators term
        (n2, n2') = (\(x, y) -> (fromIntegral x, fromIntegral y)) 
                    $ halsteadOperands term
        n = n1 + n2
        bigN = n1' + n2'
        bigN' = (n1 * logBase 2 n1) + (n2 * logBase 2 n2)
        volume = bigN * logBase 2 n
        difficulty = (n1 / 2) * (n2' / n2)
        effort = difficulty * volume
        time = effort / 18
        bugs = effort ** (2/3) / 3000
    in Map.fromList [("n1", n1),
                     ("n2", n2),
                     ("N1", n1'),
                     ("N2", n2'),
                     ("n", n),
                     ("N", bigN),
                     ("N'", bigN'),
                     ("V", volume),
                     ("D", difficulty),
                     ("E", effort),
                     ("T", time),
                     ("B", bugs)]


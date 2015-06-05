module Metrics 
( countOfSexprs
, countOfLines
, countOfLists
, countOfFunctions
, averageFunctionSize
, halsteadOperators
, halsteadOperands
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



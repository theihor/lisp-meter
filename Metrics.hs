module Metrics 
( countOfSexprs
, countOfLines
, countOfLists
, countOfFunctions
, averageFunctionSize
, halsteadMetrics
, callGraph
, cyclomaticComlexity
, recursiveComplexity
) where

import Parser
import Utils
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- size metrics
countOfLines :: String -> Int
countOfLines input = length $ lines input

countOfSexprs :: [Node] -> Int
countOfSexprs term = foreachCount term ["_"]

countOfLists :: [Node] -> Int
countOfLists term = foreachCount term ["(_*)"]

countOfFunctions :: [Node] -> Int
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
    in (length uniqueOperators, length allOperators)

halsteadOperands :: [Node] -> (Int, Int)
halsteadOperands term =
    let allLists = foreachCollect term ["(_*)"]
        allOperands = foldl (++) [] $ map (\n -> tail $ nodeComponents n) allLists
        uniqueOperands = removeDuplicates allOperands
    in (length uniqueOperands, length allOperands)

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
-- call graph
callGraph :: [Node] -> Map.Map String [String]
callGraph term =
    let allFuncs = foreachCollect term ["(defun _*)"]
        allFuncNames = map funcName allFuncs
    in foldl (\m f -> putInGraph m f allFuncNames) Map.empty allFuncs
    where
        funcName func = let (Sym name) = (nodeComponents func) !! 1 in name
        putInGraph m func allFuncNames =
              let fName = funcName func
                  fBody = functionBody func 
                  calls = foreachCollect fBody ["(_*)"]
                  called = map (\(Sym n) -> n) 
                               $ filter nodeSymbol 
                               $ map nodeHead 
                               $ filter (\call -> not $ (==) call $ parse' "()") calls
                  connected =  filter (\f -> elem f allFuncNames) called
              in Map.insert fName (removeDuplicates connected) m

-- complexity
cyclomaticComlexity :: [Node] -> Int
cyclomaticComlexity term =
    (+) 1 $ foreachCount term ["(if _*)",
                               "(when _*)",
                               "(cond _*)",
                               "(loop _*)",
                               "(map _*)",
                               "(mapc _*)",
                               "(mapcar _*)",
                               "(reduce _*)",
                               "(dolist _*)",
                               "(and _*)",
                               "(or _*)"]

-- recursiveComplexity :: [Node] -> Int
recursiveComplexity term =
    let cg = callGraph term
        n = Map.size cg
        e = foldl (+) 0 
            $ map snd 
            $ Map.toList 
            $ Map.map (\edges -> length edges) cg
        p' = p cg (roots cg) [] 0
    in e - n + p' 
    where
        dfs g u seen =
            let vs = Maybe.fromJust $ Map.lookup u g
            in if vs == [] then seen
               else foldl (\seen v -> 
                              if v `elem` seen then seen
                              else dfs g v (v : seen)) 
                          seen vs
        p _ [] _ acc = acc
        p g (u:us) seen acc
            | length seen == Map.size g = acc
            | u `elem` seen = p g us seen acc
            | otherwise =
                p g us (dfs g u (u:seen)) (acc + 1)
        roots g =
            let toNodes = foldl (++) [] $ map snd $ Map.toList g
            in filter (\n -> not $ n `elem` toNodes) $ map fst $ Map.toList g

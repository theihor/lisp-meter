module Utils
( nodeEq'
, forthis
, foreachCount
, foreachCollect
, nodeSymbol
, removeDuplicates
) where

import Parser

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f xs = takeWhile (\x -> not $ f x) xs

nodeEq' :: Node -> Node -> Bool
nodeEq' _ (Sym "_") = True
nodeEq' (Sym "_") _ = True
nodeEq' (Lisp n1) (Lisp n2) 
    | elem s n1 && elem s n2 
        = let n1' = takeUntil (== s) n1
              n2' = takeUntil (== s) n2 in
            all eq $ zip n1' n2'
    | elem s n1
        = let n1' = takeUntil (== s) n1 in
            if length n1' > length n2
            then False
            else all eq $ zip n1' n2
    | elem s n2
        = let n2' = takeUntil (== s) n2 in
            if length n1 < length n2'
            then False
            else all eq $ zip n1 n2'
    | otherwise
        = (length n1 == length n2)
          && (all eq $ zip n1 n2)
    where s = Sym "_*"
          eq = (\(x, y) -> nodeEq' x y)
nodeEq' n1 n2 = n1 == n2

forthis :: Node -> [(String, a)] -> a
forthis node (rule : rules) =
    let expectedNode = parse' (fst rule) in
        if nodeEq' node expectedNode
        then snd rule
        else forthis node rules


foreachCount :: [Node] -> [String] -> Integer
foreachCount [] _ = 0
foreachCount _ [] = 0
foreachCount (node : nodes) rules =
    (forthis node $ (zip rules $ repeat 1) ++ [("_", 0)]) + 
    (foreachCount (nodeComponents node) rules)  +
    (foreachCount nodes rules)

foreachCollect :: [Node] -> [String] -> [Node]
foreachCollect [] _ = []
foreachCollect _ [] = []
foreachCollect (node : nodes) rules =
    (forthis node $ (zip rules $ repeat [node]) ++ [("_", [])]) ++ 
    (foreachCollect (nodeComponents node) rules) ++
    (foreachCollect nodes rules)

nodeSymbol :: Node -> Bool
nodeSymbol (Sym _) = True
nodeSymbol (Lisp _) = False

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates lst = 
    foldr (\x seen -> if x `elem` seen 
                      then seen else x : seen) 
          [] lst
                             

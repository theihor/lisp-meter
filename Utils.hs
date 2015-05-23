module Utils
( nodeEq'
, forthis
, foreachCount
, foreachCollect
) where

import Parser

nodeEq :: Node -> Node -> Bool
nodeEq (Sym n1) (Lisp n2) = False
nodeEq (Lisp n1) (Sym n2) = False
nodeEq (Sym n1) (Sym n2) = n1 == n2
nodeEq (Lisp n1) (Lisp n2) = 
    ((length n1) == (length n2)) &&
    (all (\(x, y) -> nodeEq x y) $ zip n1 n2)

nodeEq' :: Node -> Node -> Bool
nodeEq' _ (Sym "_") = True
nodeEq' (Sym "_") _ = True
nodeEq' (Lisp n1) (Lisp n2) =
    ((length n1) == (length n2)) &&
    (all (\(x, y) -> nodeEq' x y) $ zip n1 n2)
nodeEq' n1 n2 = nodeEq n1 n2

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

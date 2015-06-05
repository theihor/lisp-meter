module Parser
( Node(Sym, Lisp)
, parseLsp
, parse'
, nodeComponents
) where

import Text.ParserCombinators.Parsec

data Node = Sym String | Lisp [Node]

instance Show Node where
    show (Sym s) = s
    show (Lisp l) = show l 

nodeEq :: Node -> Node -> Bool
nodeEq (Sym n1) (Lisp n2) = False
nodeEq (Lisp n1) (Sym n2) = False
nodeEq (Sym n1) (Sym n2) = n1 == n2
nodeEq (Lisp n1) (Lisp n2) = 
    ((length n1) == (length n2)) &&
    (all (\(x, y) -> nodeEq x y) $ zip n1 n2)

instance Eq Node where
    n1 == n2 = nodeEq n1 n2

lspFile :: GenParser Char st [Node]
lspFile = manyTill lisp eof

lisp :: GenParser Char st Node
lisp = do skippable
          result <- (sexpr <|> symbol)
          skippable
          return result

sexpr :: GenParser Char st Node
sexpr = 
    do char '(' 
       result <- many lisp
       char ')' >> skippable
       return $ Lisp result

symbol :: GenParser Char st Node
symbol = 
    do result <- many1 (noneOf "()\n\t; ")
       return $ Sym result

skippable :: GenParser Char st ()
skippable = skipMany ((space >> return ()) <|> comment) 

comment :: GenParser Char st () 
comment = commentLine <|> commentBlock

commentLine :: GenParser Char st ()
commentLine = 
    do char ';' 
       manyTill anyChar (char '\n')
       return ()

commentBlock :: GenParser Char st ()
commentBlock =
    do (try (string "#|"))
       manyTill anyChar (try (string "|#"))
       return ()

parseLsp :: String -> Either ParseError [Node]
parseLsp input = parse lspFile "(unknown)" input

parse' :: String -> Node
parse' input = 
    let node = parse lisp "(unknown)" input in
        either (\_ -> Sym "") (\x -> x) node

nodeComponents :: Node -> [Node]
nodeComponents (Sym sym) = []
nodeComponents (Lisp lisp) = lisp


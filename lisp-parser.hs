import Text.ParserCombinators.Parsec

data Node = Sym String | Lisp [Node]

instance Show Node where
    show (Sym s) = s
    show (Lisp l) = show l 

lspFile :: GenParser Char st [Node]
lspFile =
    do result <- many lisp
       eof
       return result

lisp :: GenParser Char st Node
lisp = spaces >> (sexpr <|> symbol)

sexpr :: GenParser Char st Node
sexpr = 
    do char '(' >> spaces
       result <- many lisp
       spaces >> char ')'
       return $ Lisp result

symbol :: GenParser Char st Node
symbol = 
    do result <- many1 (noneOf "()\n\t ")
       return $ Sym result


parseLsp :: String -> Either ParseError [Node]
parseLsp input = parse lspFile "(unknown)" input


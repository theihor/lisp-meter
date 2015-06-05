import Parser
import Utils 
import Metrics
import System.IO
import Control.Monad
import qualified System.Directory as Dir
import qualified Data.Map as Map

lispFile :: String -> Bool
lispFile p = ((drop (length p - 4) p) == ".lsp") 
             || ((drop (length p - 5) p) == ".lisp")

curOrPrevDir :: String -> Bool
curOrPrevDir "." = True
curOrPrevDir ".." = True
curOrPrevDir _ = False

getLispFiles :: String -> IO [String]
getLispFiles path = do
    print path
    names <- Dir.getDirectoryContents path
    let names' = filter (\n -> not $ curOrPrevDir n) names
        paths = map (\n -> path ++ "/" ++ n) names' in
      do lispFiles <- filterM (\p -> do e <- Dir.doesFileExist p 
                                        return $ e && lispFile p) 
                              paths
         dirs <- filterM Dir.doesDirectoryExist paths
         lispFilesFromDirs <- mapM getLispFiles dirs 
         let result = foldl (++) lispFiles lispFilesFromDirs in
           do return result

parseFile :: String -> IO [Node]
parseFile path = do 
    content <- readFile path
    let nodes = parseLsp content in
        return $ either (\_ -> [Sym "PARSE-ERROR"]) (\x -> x) nodes

main = do
    contents <- readFile "test.lsp"
    putStrLn $ show $ parseLsp contents

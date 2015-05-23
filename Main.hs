import Parser
import Utils 
import qualified Data.Map as Map
import System.IO
import qualified System.Directory as Dir
import Control.Monad

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
      do lispFiles <- filterM (\p -> do e <- Dir.doesFileExist p; return $ e && lispFile p) paths
         dirs <- filterM Dir.doesDirectoryExist paths
         lispFilesFromDirs <- mapM getLispFiles dirs 
         let result = foldl (++) lispFiles lispFilesFromDirs in
           do return result
       

main = do
    contents <- readFile "test.lsp"
    putStrLn $ show $ parseLsp contents

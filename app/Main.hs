module Main where
import Lexer ( scan )
import Parser ( parseProg )
import System.Environment ( getArgs )

main :: IO ()
main = do
    [filename] <- getArgs
    source <- readFile filename
    let scanned = scan source
    let parsed = parseProg scanned
    print scanned
    print parsed
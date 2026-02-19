module Main where
import Lexer ( scan )
import System.Environment ( getArgs )

main :: IO ()
main = do
    [filename] <- getArgs
    source <- readFile filename
    print $ scan source

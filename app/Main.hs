module Main where
import Lexer ( scan )
import Parser ( parseProg, pretty )
import Codegen ( emit )
import System.Environment ( getArgs )

main :: IO ()
main = do
    [filename] <- getArgs
    source <- readFile filename
    let scanned = scan source
    let Just (parsed, _) = parseProg scanned
    let asm = emit parsed
    putStrLn $ "Scanned tokens: " ++ show scanned
    putStrLn ""
    -- putStrLn $ "AST: " ++ show parsed
    putStrLn $ pretty 0 parsed
    putStrLn ""
    putStrLn $ unlines asm
module Main where

-- import AST ( pretty )
import Lexer ( scan )
import Parser ( parse, parseProg )
import Codegen ( emit )
import System.Environment ( getArgs )
import System.FilePath ( dropExtension )
import System.Process ( callProcess )

main :: IO ()
main = do
    [filename] <- getArgs
    source <- readFile filename
    let scanned = scan source
    let baseName = dropExtension filename
    let asmFile = baseName ++ ".s"

    case parse parseProg scanned of
        Just (parsed, _) -> do
            let asm = emit parsed
            writeFile asmFile $ unlines asm
            callProcess "gcc" [asmFile, "-o", baseName]
            -- putStrLn $ "Scanned tokens: " ++ show scanned
            -- putStrLn ""
            -- putStrLn $ "AST: " ++ show parsed
            -- putStrLn $ pretty 0 parsed
            -- putStrLn ""
            putStrLn $ unlines asm
        _ -> error "Parse failed"
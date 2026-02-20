module Lexer where
import Data.Char ( isAlpha, isAlphaNum, isDigit, isSpace )

data Token = OBrace | CBrace |
             OParen | CParen |
             Semicolon |
             IntKeyword | ReturnKeyword |
             Identifier String |
             IntegerLiteral Integer |
             Neg |
             BitComp |
             Not 
             deriving Show

scan :: String -> [Token]
scan "" = []
scan xs | isSpace $ head xs = scan $ tail xs
scan ('{':xs) = OBrace : scan xs
scan ('}':xs) = CBrace : scan xs
scan ('(':xs) = OParen : scan xs
scan (')':xs) = CParen : scan xs
scan (';':xs) = Semicolon : scan xs
scan ('-':xs) = Neg : scan xs
scan ('~':xs) = BitComp : scan xs
scan ('!':xs) = Not : scan xs
scan xs | word == "" = error $ "Illegal Character: " ++ [head xs]
        | word == "int" = IntKeyword : scan rest
        | word == "return" = ReturnKeyword : scan rest
        | isAlpha $ head word = Identifier word : scan rest
        | otherwise = let
            (num, rrest) = span isDigit word
            in IntegerLiteral (read num :: Integer) : scan (rrest ++ rest)
    where
        (word, rest) = span isAlphaNum xs
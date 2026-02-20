module Lexer where
import Data.Char ( isAlpha, isAlphaNum, isDigit, isSpace )

data Token = OBrace | CBrace |
             OParen | CParen |
             Semicolon |
             IntKeyword | ReturnKeyword |
             Identifier String |
             IntegerLiteral Integer |
             Plus | Minus | Asterisk | ForwardSlash |
             Tilde |
             Bang
             deriving (Show, Eq)

keyword :: String -> Token
keyword "int" = IntKeyword
keyword "return" = ReturnKeyword
keyword name = Identifier name

scan :: String -> [Token]
scan "" = []
scan (x:xs) | isSpace $ x = scan $ xs
scan ('{':xs) = OBrace : scan xs
scan ('}':xs) = CBrace : scan xs
scan ('(':xs) = OParen : scan xs
scan (')':xs) = CParen : scan xs
scan (';':xs) = Semicolon : scan xs
scan ('-':xs) = Minus : scan xs
scan ('~':xs) = Tilde : scan xs
scan ('!':xs) = Bang : scan xs
scan ('+':xs) = Plus : scan xs
scan ('*':xs) = Asterisk: scan xs
scan ('/':xs) = ForwardSlash: scan xs
scan (x:xs) | isAlpha x = keyword word : scan rest where
    (word, rest) = span isAlphaNum (x:xs)
scan (x:xs) | isDigit x = IntegerLiteral (read num) : scan rest where
    (num, rest) = span isDigit (x:xs)
scan (x:_) = error $ "Illegal character: " ++ [x]
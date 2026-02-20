module Parser where
import Lexer ( Token(..) )

data Exp = Const Integer deriving (Show, Eq)
data Statement = Return Exp deriving (Show, Eq)
data FuncDecl = Func String Statement deriving (Show, Eq)
data Prog = Prog FuncDecl deriving (Show, Eq)

newtype Parser a = P ([Token] -> Maybe (a, [Token]))

parse :: Parser a -> [Token] -> Maybe (a, [Token])
parse (P parser) = parser

-- instance Functor Parser where
--     fmap f pa = case (\input -> parse pa input) of
--         Nothing -> P Nothing
--         Just (a, rest) -> P (f a, rest)


parseExp :: [Token] -> Maybe (Exp, [Token])
parseExp [] = Nothing
parseExp (x:xs) = case x of
    IntegerLiteral num -> Just (Const num, xs)
    _ -> Nothing

parseStatement :: [Token] -> Maybe (Statement, [Token])
parseStatement [] = Nothing
parseStatement (x:xs) = case x of
    ReturnKeyword -> let res = parseExp xs in case res of
        Just (expr, rest) -> let end = head rest in case end of
            Semicolon -> Just (Return expr, tail rest)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

parseFuncDecl :: [Token] -> Maybe (FuncDecl, [Token])
-- IntKeyword, Identifier, OParen, CParen, OBrace, PARSE_STATEMENT, CBrace
parseFuncDecl [] = Nothing
parseFuncDecl (ik:id:op:cp:ob:xs) = case (ik, id, op, cp, ob) of
    (IntKeyword, Identifier name, OParen, CParen, OBrace) -> let res = parseStatement xs in case res of
        Just (statement, rest) -> let end = head rest in case end of
            CBrace -> Just (Func name statement, tail rest)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
parseFuncDecl _ = Nothing

parseProg :: [Token] -> Maybe (Prog, [Token])
parseProg [] = Nothing
parseProg xs = case parseFuncDecl xs of
    Just (func, []) -> Just (Prog func, [])
    Just (_, _) -> Nothing
    _ -> Nothing
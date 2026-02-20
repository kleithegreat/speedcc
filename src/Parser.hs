module Parser where
import Lexer ( Token(..) )
import AST (Prog (..), FuncDecl (..), Statement (..), Exp (..), UnaryOperator (..))

newtype Parser a = P ([Token] -> Maybe (a, [Token]))

parse :: Parser a -> [Token] -> Maybe (a, [Token])
parse (P parser) = parser

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f pa = P $ \input -> case parse pa input of
        Just (a, rest) -> Just (f a, rest)
        _ -> Nothing

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P (\input -> Just (a, input))

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> pa = P $ \input -> case parse pf input of
        Just (f, rest) -> parse (fmap f pa) rest
        _ -> Nothing

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= f = P $ \input -> case parse pa input of
        Just (a, rest) -> parse (f a) rest
        _ -> Nothing

check :: (Token -> Maybe a) -> Parser a
check f = P $ \toks -> case toks of
    (t:ts) -> case f t of
        Just a -> Just (a, ts)
        Nothing -> Nothing
    [] -> Nothing

orCombinator :: Parser a -> Parser a -> Parser a
orCombinator pa pb = P $ \toks -> case parse pa toks of
    Just (a, rest) -> Just (a, rest)
    Nothing -> parse pb toks

parseIntLiteral :: Parser Exp
parseIntLiteral = check $ \tok -> case tok of
    IntegerLiteral num -> Just (Const num)
    _ -> Nothing

parseUnaryOperator :: Parser Exp
parseUnaryOperator = do
    operator <- check $ \tok -> case tok of
        Neg -> Just Negate
        BitComp -> Just Complement
        Not -> Just LogicalNot
        _ -> Nothing
    next <- parseExp
    return $ UnOp operator next

parseExp :: Parser Exp
parseExp = parseUnaryOperator `orCombinator` parseIntLiteral

parseStatement :: Parser Statement
parseStatement = do
    _ <- check $ \tok -> case tok of
        ReturnKeyword -> Just Return
        _ -> Nothing
    val <- parseExp
    _ <- check $ \tok -> case tok of
        Semicolon -> Just ()
        _ -> Nothing
    return $ Return val

-- parseStatement :: [Token] -> Maybe (Statement, [Token])
-- parseStatement [] = Nothing
-- parseStatement (x:xs) = case x of
--     ReturnKeyword -> let res = parseExp xs in case res of
--         Just (expr, rest) -> let end = head rest in case end of
--             Semicolon -> Just (Return expr, tail rest)
--             _ -> Nothing
--         _ -> Nothing
--     _ -> Nothing
-- 
-- parseFuncDecl :: [Token] -> Maybe (FuncDecl, [Token])
-- -- IntKeyword, Identifier, OParen, CParen, OBrace, PARSE_STATEMENT, CBrace
-- parseFuncDecl [] = Nothing
-- parseFuncDecl (ik:id:op:cp:ob:xs) = case (ik, id, op, cp, ob) of
--     (IntKeyword, Identifier name, OParen, CParen, OBrace) -> let res = parseStatement xs in case res of
--         Just (statement, rest) -> let end = head rest in case end of
--             CBrace -> Just (Func name statement, tail rest)
--             _ -> Nothing
--         _ -> Nothing
--     _ -> Nothing
-- parseFuncDecl _ = Nothing
-- 
-- parseProg :: [Token] -> Maybe (Prog, [Token])
-- parseProg [] = Nothing
-- parseProg xs = case parseFuncDecl xs of
--     Just (func, []) -> Just (Prog func, [])
--     Just (_, _) -> Nothing
--     _ -> Nothing
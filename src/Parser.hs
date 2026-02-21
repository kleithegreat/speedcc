module Parser where
import Lexer ( Token(..) )
import AST (Prog (..), FuncDecl (..), Statement (..), Exp (..), UnaryOperator (..), BinaryOperator (..))

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

eof :: Parser ()
eof = P $ \toks -> case toks of
    [] -> Just ((), [])
    _  -> Nothing

parseFactor :: Parser Exp
parseFactor = do
    _ <- check $ \tok -> case tok of
        OParen -> Just ()
        _ -> Nothing
    middle <- parseExp
    _ <- check $ \tok -> case tok of
        CParen -> Just ()
        _ -> Nothing
    return middle $ `orCombinator` parseUnaryOperator `orCombinator` parseIntLiteral

parseIntLiteral :: Parser Exp
parseIntLiteral = check $ \tok -> case tok of
    IntegerLiteral num -> Just (Const num)
    _ -> Nothing

parseUnaryOperator :: Parser Exp
parseUnaryOperator = do
    operator <- check $ \tok -> case tok of
        Minus -> Just Negate
        Tilde -> Just Complement
        Bang -> Just LogicalNot
        _ -> Nothing
    next <- parseFactor
    return $ UnOp operator next

parseExp :: Parser Exp
parseExp = parseUnaryOperator `orCombinator` parseIntLiteral

parseStatement :: Parser Statement
parseStatement = do
    _ <- check $ \tok -> case tok of
        ReturnKeyword -> Just ()
        _ -> Nothing
    val <- parseExp
    _ <- check $ \tok -> case tok of
        Semicolon -> Just ()
        _ -> Nothing
    return $ Return val

parseFuncDecl :: Parser FuncDecl
parseFuncDecl = do
    _ <- check $ \tok -> case tok of
        IntKeyword -> Just ()
        _ -> Nothing
    funcName <- check $ \tok -> case tok of
        Identifier n -> Just n
        _ -> Nothing
    _ <- check $ \tok -> case tok of
        OParen -> Just ()
        _ -> Nothing
    _ <- check $ \tok -> case tok of
        CParen -> Just ()
        _ -> Nothing
    _ <- check $ \tok -> case tok of
        OBrace -> Just ()
        _ -> Nothing
    statement <- parseStatement
    _ <- check $ \tok -> case tok of
        CBrace -> Just ()
        _ -> Nothing
    return $ Func funcName statement

parseProg :: Parser Prog
parseProg = do
    func <- parseFuncDecl
    eof
    return $ Prog func

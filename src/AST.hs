module AST where

data Exp = BinOp BinaryOperator Exp Exp | UnOp UnaryOperator Exp | Const Integer deriving (Show, Eq)
data UnaryOperator = Negate | Complement | LogicalNot deriving (Show, Eq)
data BinaryOperator = Add | Subtract | Multiply | Divide deriving (Show, Eq)
data Statement = Return Exp deriving (Show, Eq)
data FuncDecl = Func String Statement deriving (Show, Eq)
data Prog = Prog FuncDecl deriving (Show, Eq)


indent :: Int -> String
indent n = replicate (n * 2) ' '

pretty :: Int -> Prog -> String
pretty n (Prog func) = indent n ++ "Program\n" ++ prettyFunc (n + 1) func

prettyFunc :: Int -> FuncDecl -> String
prettyFunc n (Func name stmt) = indent n ++ "Function: " ++ name ++ "\n" ++ prettyStmt (n + 1) stmt

prettyStmt :: Int -> Statement -> String
prettyStmt n (Return e) = indent n ++ "Return\n" ++ prettyExp (n + 1) e

prettyExp :: Int -> Exp -> String
prettyExp n (Const num) = indent n ++ "Const: " ++ show num
prettyExp n (UnOp op e) = indent n ++ "UnaryOp: " ++ show op ++ "\n" ++ prettyExp (n + 1) e
prettyExp n (BinOp op e1 e2) = indent n ++ "BinaryOp" ++ show op ++ "\n" ++ prettyExp (n + 1) e1 ++ "\n" ++ prettyExp (n + 1) e2
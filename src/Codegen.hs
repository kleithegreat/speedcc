module Codegen where
import AST (Prog (..), FuncDecl (..), Statement (..), Exp (..), UnaryOperator (..))

emitUnOp :: UnaryOperator -> [String]
emitUnOp Negate = ["negq %rax"]
emitUnOp Complement = ["notq %rax"]
emitUnOp LogicalNot = ["cmpq $0, %rax", "movq $0, %rax", "sete %al"]

emitExp :: Exp -> [String]
emitExp (Const num) = ["movq $" ++ show num ++ ", %rax"]
emitExp (UnOp op inner) = emitExp inner ++ emitUnOp op

emitStatement :: Statement -> [String]
emitStatement (Return expr) = emitExp expr ++ ["ret"]

emitFuncDecl :: FuncDecl -> [String]
emitFuncDecl (Func name stmt) = [".globl " ++ name] ++ [name ++ ":"] ++ emitStatement stmt

emit :: Prog -> [String]
emit (Prog func) = emitFuncDecl func
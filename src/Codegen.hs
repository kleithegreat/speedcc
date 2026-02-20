module Codegen where
import AST (Prog (..), FuncDecl (..), Statement (..), Exp (..))

emitExp :: Exp -> [String]
emitExp (Const num) = ["movq $" ++ show num ++ ", %rax"]

emitStatement :: Statement -> [String]
emitStatement (Return expr) = emitExp expr ++ ["ret"]

emitFuncDecl :: FuncDecl -> [String]
emitFuncDecl (Func name stmt) = [".globl " ++ name] ++ [name ++ ":"] ++ emitStatement stmt

emit :: Prog -> [String]
emit (Prog func) = emitFuncDecl func
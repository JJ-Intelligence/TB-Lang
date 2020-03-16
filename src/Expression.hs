module Expression where

import qualified Data.Map.Strict as Map

-- **CESK machine types**

-- Environment - A mapping of functions and variables to Closures (which maps an Expression to an Environment).
type Address = Int
data Scope = Local | Global deriving (Eq, Show)
type Environment = Map.Map String (Address, Scope)
type Store = Map.Map Address ExprValue

-- Kontinuation - A stack containing Frames showing what to do.
type Kon = [ Frame ]

-- Frame - Data structures to be put onto the Kontinuation.
data BinOpFrame = BinCompOp ExprComp Expr Environment -- Frame for a binary comparison operation - e.g. [-] == e2
                | BinSeqOp Expr -- Frame for a binary sequence operation - e.g. [-] ; e2
                | BinConsOp Expr Environment
                | BinMathOp ExprMath Expr Environment
                | BinParameters Expr Environment
                | BinFuncCallFrame String Expr Environment
                deriving (Eq, Show)

data TerOpFrame = TerIfOp Expr (Maybe ExprElif) Environment -- Frame for a ternary if statement operation - e.g. if [-] then e1 e2 (e2 is the else/elif)
                | TerWhileOp Expr Expr Environment -- Frame for a ternary while loop operation - e.g. while c then e1
                deriving (Eq, Show)

data Frame = HBinOp BinOpFrame
           | BinOpH BinOpFrame
           | HTerOp TerOpFrame
           | TerOpH TerOpFrame
           | DefVarFrame String Environment
           | FuncCallFrame String Environment
           | ReturnFrame
           | Done 
           deriving (Eq, Show)

-- State - The current state/configuration of the CESK machine.
type State = (Expr, Environment, Store, Kon)

-- **Expression types**

-- Data types.
data Type = TInt 
          | TBool 
          | TEmpty 
          | TList Type 
          | TConflict deriving (Eq)

instance Show Type where 
    show TInt = "Int"
    show TBool = "Boolean"
    show TEmpty = ""
    show (TList t) = "[" ++ (show t) ++ "]" 
    show TConflict = "Conflict"

-- **Expression type returned by Parser**

-- Elif part of an If statement.
data ExprElif = Elif Expr Expr (Maybe ExprElif)
              | Else Expr
              deriving (Eq)

instance Show ExprElif where
  show (Elif c e1 Nothing) = "elif (" ++ (show c) ++ ") {\n" ++ (show e1) ++ "\n}\n" 
  show (Elif c e1 (Just e2)) = "elif (" ++ (show c) ++ ") {\n" ++ (show e1) ++ "\n} " ++ (show e2) 
  show (Else e1) = "else {\n" ++ (show e1) ++ "\n}\n"
-- Literals.
data ExprLiteral = EInt Int
                 | EBool Bool
                 | Empty
                 | ENone
                 deriving (Eq)

instance Show ExprLiteral where
  show (EInt n) = show n
  show (EBool b) = show b
  show Empty = "[]"
  show ENone = "null"

data ExprValue = VInt Int
               | VBool Bool
               | VList [ ExprValue ]
               | VNone
               | VFunc [ (Parameters, Expr) ]
               | CallStack [ (Environment, Store, Kon) ]
               deriving (Eq)

instance Show ExprValue where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VList []) = "[]"
  show (VList ((VList xs):ls)) = (show (VList xs)) ++ "\n" ++ (show (VList ls)) 
  show (VList xs) = filter (/='"') (helper (map (show) xs))
    where helper [] = " "
          helper [y] = (show y) ++ " "
          helper (x:y:xs) = (show x) ++ " " ++ (helper (y:xs))
  show VNone = "null"
  show (VFunc xs) = "VFunc " ++ (show xs)
  show (CallStack xs) = "CallStack " ++ (show xs)

-- Binary Operation.
data BinOp = CompOp ExprComp Expr Expr
           | MathOp ExprMath Expr Expr
           | Cons Expr Expr
           deriving (Eq)

instance Show BinOp where
  show (CompOp op e1 e2) = (show e1) ++ " " ++ (show op) ++ " " ++ (show e2)
  show (MathOp op e1 e2) = (show e1) ++ " " ++ (show op) ++ " " ++ (show e2)
  show (Cons e1 e2) = (show e1) ++ " : " ++ (show e2)

-- Comparison operations.
data ExprComp = Equality
              | And 
              | Or
              | LessThan
              | GreaterThan
              deriving (Eq)

instance Show ExprComp where
  show Equality = "=="
  show And = "&&"
  show Or = "||"
  show LessThan = "<"
  show GreaterThan = ">"

-- Mathematical operations.
data ExprMath = Plus
              | Min
              | Mult
              | Div
              | Exp
              | Mod
              deriving (Eq)

instance Show ExprMath where
  show Plus = "+"
  show Min = "-"
  show Mult = "*"
  show Div = "/"
  show Exp = "^"
  show Mod = "%"

data Parameters = FuncParam Expr Parameters
                | FuncParamEnd
                deriving (Eq)

instance Show Parameters where
  show (FuncParam e1 FuncParamEnd) = show e1
  show (FuncParam e1 e2) = (show e1) ++ ", " ++ (show e2)
  show (FuncParamEnd) = ""

data Expr = If Expr Expr (Maybe ExprElif)
          | While Expr Expr
          | Func Parameters Expr
          | Return Expr
          | FuncCall String Parameters
          | Literal ExprLiteral
          | Value ExprValue
          | Op BinOp
          | DefVar String Expr
          | Var String
          | Seq Expr Expr
          | FuncBlock Expr
          deriving (Eq)

instance Show Expr where
  show (If c e1 Nothing) = "if (" ++ (show c) ++ ") {\n" ++ (show e1) ++ "\n}"
  show (If c e1 (Just e2)) = "if (" ++ (show c) ++ ") {\n" ++ (show e1) ++ "\n} " ++ (show e2)
  show (While c e1) = "while (" ++ (show c) ++ ") {\n" ++ (show e1) ++ "\n} "
  show (DefVar s (Func ps e1)) = "func " ++ s ++ " (" ++ (show ps) ++ ") = {\n" ++ (show e1) ++ "\n}" 
  show (FuncCall s ps)  = s ++ "(" ++ (show ps) ++ ")"
  show (Return e1) = "return " ++ (show e1)
  show (Literal l) = show l
  show (Value v) = show v
  show (Op op) = show op
  show (DefVar s e1) = s ++ " = " ++ (show e1)
  show (Var s) = s
  show (Seq e1 e2) = (show e1) ++ ";\n" ++ (show e2)

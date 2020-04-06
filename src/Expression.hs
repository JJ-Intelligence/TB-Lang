module Expression where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Lazy as MapL
import System.IO (hPutStrLn, stderr)

-- **CESK machine types**

-- Environment - A mapping of functions and variables to Closures (which maps an Expression to an Environment).
type Address = Int
type Environment = Map.Map String Address
type Store = MapL.IntMap ExprValue

-- Kontinuation - A stack containing Frames showing what to do.
type Kon = [ Frame ]

-- Frame - Data structures to be put onto the Kontinuation.
data BinOpFrame = BinCompOp ExprComp Expr Environment -- Frame for a binary comparison operation - e.g. [-] == e2
                | BinSeqOp Expr -- Frame for a binary sequence operation - e.g. [-] ; e2
                | BinConsOp Expr Environment
                | BinMathOp ExprMath Expr Environment
                | BinFuncCallFrame String Expr Environment
                deriving (Eq, Show)

data TerOpFrame = TerIfOp Expr (Maybe ExprElif) -- Frame for a ternary if statement operation - e.g. if [-] then e1 e2 (e2 is the Maybe else/elif)
                | TerWhileOp Expr Expr -- Frame for a ternary while loop operation - e.g. while c e1
                | TerForInit Expr Expr Expr Expr -- Frame for a ternary for loop operation - holds the initialisation, condition, increment and expression, e.g. for (i; c; n) e1
                | TerForOp Expr Expr Expr -- Frame for a ternary for loop operation - holds the condition, increment and the expression.
                deriving (Eq, Show)

data Frame = HBinOp BinOpFrame
           | BinOpH BinOpFrame
           | HTerOp TerOpFrame
           | TerOpH TerOpFrame
           | TerOp_H TerOpFrame
           | DefLocalVarFrame String
           | DefGlobalVarFrame String
           | DefPointerVarFrame String
           | AddressExprFrame
           | FuncCallFrame String
           | ReturnFrame
           | FuncBlockFrame
           | ThrownException ExprValue
           | CatchFrame [ExprValue] Expr Environment
           | Done 
           deriving (Eq, Show)

-- State - The current state/configuration of the CESK machine.
type State = (Expr, Environment, Store, Address, Kon)

-- **Expression types**

-- Data types.
data Type = TFunc [Type] Type [(String, TypeClass)]
          | TInt 
          | TBool 
          | TEmpty 
          | TNone
          | TIterable Type -- TList and TStream are both iterable types. This takes in the type of the Stream/List.
          | TList Type
          | TStream -- Streams will always be of type Int.
          | TRef Type
          | TGeneric String
          | TConflict
          | TParamList
          | TException
          deriving (Eq, Ord)

instance Show Type where 
    show (TFunc ps out cs) = "TFunc " ++ (show ps) ++ " " ++ (show out) ++ " " ++ (show cs)
    show TInt = "Int"
    show TBool = "Boolean"
    show TEmpty = ""
    show TNone = "null"
    show (TIterable t) = "TIterable " ++ (show t)
    show (TList x) = "TList ["++(show x)++"]" 
    show TStream = "Stream"
    show (TRef x) = "Ref " ++ (show x)
    show (TGeneric s) = s
    show TConflict = "Conflict"
    show TParamList = "TParamList"
    show TException = "Exception"

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

-- [Position] is the call stack
data Exception = EmptyListException
               | IndexOutOfBoundException
               | StreamOutOfInputException
               | InvalidParameterException
               | InvalidInputException
               | NonExhaustivePatternException
               deriving (Eq, Show)

data ExprValue = VInt Int
               | VBool Bool
               | VVar String
               | VPointer String
               | VList Type [ ExprValue ]
               | VPointerList Type [ ExprValue ]
               | VStream Int [ ExprValue ]
               | VNone
               | VFunc Type [([ExprValue], Expr)]
               | VFuncUnTypedDef [([ExprValue], Expr)]
               | VRef Address
               | VException Exception
               | GlobalEnv Environment
               deriving (Eq)

instance Show ExprValue where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VList _ xs) = (foldr (\x acc -> x ++ "\n" ++ acc) "" $ init xs') ++ (last xs')
    where
        xs' = map show xs
  -- show (VList t xs) = "VList " ++ (show t) ++ " " ++ (show xs)
  -- show (VList _ []) = ""
  -- show (VList _ [x]) = show x
  -- show (VList t (x:xs)) = (show x) ++ "\n" ++ (show $ VList t xs)
  show (VPointerList t xs) = "VPointerList " ++ (show t) ++ " " ++ (show xs)
  show (VStream n xs) = "VStream " ++ (show n) ++ " " ++ (show xs)
  show VNone = "null"
  show (VFunc ts xs) = "VFunc " ++ (show ts) ++ " " ++ (show xs)
  show (VRef n) = "VRef " ++ (show n)
  show (GlobalEnv env) = "GlobalEnv " ++ (show env)
  show (VVar s) = "Var " ++ s
  show (VPointer s) = "VPointer " ++ s
  show (VException ex) = "Exception " ++ (show ex)

instance Ord ExprValue where
  (<) (VInt a) (VInt b) = a < b
  (<) _ _ = error "ExprValue can not be used in comparison operation."

  (<=) (VInt a) (VInt b) = a <= b
  (<=) _ _ = error "ExprValue can not be used in comparison operation."

  (>) (VInt a) (VInt b) = a > b
  (>) _ _ = error "ExprValue can not be used in comparison operation."

  (>=) (VInt a) (VInt b) = a >= b
  (>=) _ _ = error "ExprValue can not be used in comparison operation."

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
              | NotEquals
              | And 
              | Or
              | LessThan
              | GreaterThan
              deriving (Eq)

instance Show ExprComp where
  show Equality = "=="
  show NotEquals = "!="
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
                deriving (Eq, Show)

-- instance Show Parameters where
--   show (FuncParam e1 FuncParamEnd) = show e1
--   show (FuncParam e1 e2) = (show e1) ++ ", " ++ (show e2)
--   show (FuncParamEnd) = ""

data Assignment = DefVar String Expr
                deriving (Eq, Show)

data TypeClass = CEq
               | CItr
               | COrd
               deriving (Eq, Ord, Show)

data Expr = If Expr Expr (Maybe ExprElif)
          | While Expr Expr
          | For Expr Expr Expr Expr
          | Func Parameters Expr
          | FuncType Parameters Expr (Maybe Parameters) -- Maybe Parameters are the Type constraints
          | ExprType Type
          | TypeConstraint TypeClass String
          | Return Expr
          | FuncCall String Parameters
          | Literal ExprLiteral
          | Value ExprValue
          | Op BinOp
          | LocalAssign Assignment
          | GlobalAssign Assignment
          | DefPointerVar String Expr
          | PointerExpr Expr
          | AddressExpr Expr
          | GlobalVar String
          | Var String
          | Seq Expr Expr
          | FuncBlock Expr
          | BuiltInFunc String [Expr]
          | TryCatch Expr Parameters Expr
          deriving (Eq, Show)

-- instance Show Expr where
--   show (If c e1 Nothing) = "if (" ++ (show c) ++ ") {\n" ++ (show e1) ++ "\n}"
--   show (If c e1 (Just e2)) = "if (" ++ (show c) ++ ") {\n" ++ (show e1) ++ "\n} " ++ (show e2)
--   show (While c e1) = "while (" ++ (show c) ++ ") {\n" ++ (show e1) ++ "\n} "
--   show (DefVar s (Func ps e1)) = "func " ++ s ++ " (" ++ (show ps) ++ ") = {\n" ++ (show e1) ++ "\n}"
--   show (FuncCall s ps)  = s ++ "(" ++ (show ps) ++ ")"
--   show (Return e1) = "return " ++ (show e1)
--   show (Literal l) = show l
--   show (Value v) = show v
--   show (Op op) = show op
--   show (DefPointerVar s e) = "*" ++ s ++ " = " ++ (show e)
--   show (PointerExpr e) = "*(" ++ (show e) ++ ")"
--   show (AddressVar s) = "&" ++ s
--   show (DefVar s e1) = s ++ " = " ++ (show e1)
--   show (Var s) = s
--   show (Seq e1 e2) = (show e1) ++ ";\n" ++ (show e2)
--   show (FuncBlock e1) = "{" ++ (show e1) ++ "}"
--   show (BuiltInFunc s _) = s

printStdErr :: String -> IO ()
printStdErr s = do
    hPutStrLn stderr s
    return ()


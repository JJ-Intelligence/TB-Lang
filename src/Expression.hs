module Expression where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Lazy as MapL
import System.IO (hPutStrLn, stderr)
import Data.Maybe

-- **CESK machine types**

-- Environment - A mapping of functions and variables to Closures (which maps an Expression to an Environment).
type Address = Int
type Environment = Map.Map String Address
type Store = MapL.IntMap ExprValue

-- Kontinuation - A stack containing Frames showing what to do.
type Kon = [ Frame ]

-- Function call stack (used in exception error printing).
type CallStack = [ Expr ]

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
           | BoolNotFrame
           | ThrownException ExprValue CallStack -- Exception and list of FuncCalls
           | CatchFrame [ExprValue] Expr Environment CallStack
           | Done 
           deriving (Eq, Show)

-- State - The current state/configuration of the CESK machine.
type State = (Expr, Environment, Store, Address, CallStack, Kon)

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
          | FuncType Parameters Expr (Maybe Parameters)
          deriving (Eq, Ord)

instance Show Type where 
    show (TFunc ps out cs) = "Function (" ++ (if ps == [] then "" else (show $ head ps) ++ (foldr (\p acc -> ", " ++ (show p)) "" (tail ps))) ++ ") -> " ++ (show out) ++ (typeCSToStr cs)
        where
            typeCSToStr [] = ""
            typeCSToStr cs = " ~ (" ++ (helper $ head cs) ++ (foldr (\c acc -> ", " ++ (helper c) ++ acc) "" (tail cs)) ++ ")"
                where
                    helper (s, tc) = (show tc) ++ " " ++ s
    show TInt = "Int"
    show TBool = "Boolean"
    show TEmpty = ""
    show TNone = "NoneType"
    show (TIterable t) = "Itr " ++ (show t)
    show (TList x) = "["++(show x)++"]" 
    show TStream = "Stream"
    show (TRef x) = "Ref " ++ (show x)
    show (TGeneric s) = s
    show TConflict = "Type Conflict"
    show TParamList = "TParamList"
    show TException = "Exception"
    show (FuncType ps out cs) = "(" ++ (show ps) ++ ") -> " ++ (show out) ++ 
      (if cs == Nothing then "" else " ~ (" ++ (show $ fromJust cs) ++ ")")

-- Elif part of an If statement.
data ExprElif = Elif Expr Expr (Maybe ExprElif) Pos
              | Else Expr
              deriving (Eq, Ord)

instance Show ExprElif where
  show (Elif c e1 Nothing _) = " elif (" ++ (show c) ++ ") {\n" ++ (tabString $ show e1) ++ "\n}" 
  show (Elif c e1 (Just e2) _) = " elif (" ++ (show c) ++ ") {\n" ++ (tabString $ show e1) ++ "\n}" ++ (show e2) 
  show (Else e1) = " else {\n" ++ (tabString $ show e1) ++ "\n}"

-- Literals.
data ExprLiteral = EInt Int
                 | EBool Bool
                 | Empty
                 | ENone
                 deriving (Eq, Ord)

instance Show ExprLiteral where
  show (EInt n) = show n
  show (EBool b) = show b
  show Empty = "[]"
  show ENone = "None"

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
  show (VVar s) = s
  show (VPointer s) = "*" ++ s
  show (VList _ []) = ""
  show (VList _ xs) = (show $ head xs) ++ (foldr (\x acc -> "\n" ++ (show x) ++ acc) "" (tail xs))
  show (VPointerList t xs) = "VPointerList " ++ (show t) ++ " " ++ (show xs)
  show (VStream n xs) = "VStream " ++ (show n) ++ " " ++ (show xs)
  show VNone = "None"
  show (VFunc ts xs) = "VFunc " ++ (show ts) ++ " " ++ (show xs)
  show (VRef n) = "Ref " ++ (show n)
  show (VException ex) = show ex
  show (GlobalEnv env) = "GlobalEnv " ++ (show env)

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
           deriving (Eq, Ord)

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
              deriving (Eq, Ord)

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
              deriving (Eq, Ord)

instance Show ExprMath where
  show Plus = "+"
  show Min = "-"
  show Mult = "*"
  show Div = "/"
  show Exp = "^"
  show Mod = "%"

data Parameters = FuncParam Expr Parameters
                | FuncParamEnd
                deriving (Eq, Ord)

instance Show Parameters where
  show (FuncParam e1 FuncParamEnd) = show e1
  show (FuncParam e1 e2) = (show e1) ++ ", " ++ (show e2)
  show (FuncParamEnd) = ""

data Assignment = DefVar String Expr Pos
                deriving (Eq, Show, Ord)

data TypeClass = CEq
               | CItr
               | COrd
               deriving (Eq, Ord, Show)

data Expr = If Expr Expr (Maybe ExprElif) Pos
          | While Expr Expr Pos
          | For Expr Expr Expr Expr Pos
          | Func Parameters Expr
          | ExprType Type
          | TypeConstraint TypeClass String
          | Return Expr Pos
          | FuncCall String Parameters Pos 
          | Literal ExprLiteral
          | Value ExprValue
          | Op BinOp Pos
          | LocalAssign Assignment
          | GlobalAssign Assignment
          | DefPointerVar String Expr
          | PointerExpr Expr
          | AddressExpr Expr Pos
          | BooleanNotExpr Expr Pos
          | GlobalVar String Pos
          | Var String Pos
          | Seq Expr Expr
          | FuncBlock Expr Pos
          | BuiltInFunc String [Expr]
          | TryCatch Expr Parameters Expr Pos
          deriving (Eq, Ord)

instance Show Expr where
  show (If c e1 Nothing _) = "if (" ++ (show c) ++ ") {\n" ++ (tabString $ show e1) ++ "\n}"
  show (If c e1 (Just e2) _) = "if (" ++ (show c) ++ ") {\n" ++ (tabString $ show e1) ++ "\n}" ++ (show e2)
  show (While c e1 _) = "while (" ++ (show c) ++ ") {\n" ++ (tabString $ show e1) ++ "\n}"
  show (For i c n e1 _) = "for ("++(show i)++" ; " ++ (show c) ++ " ; " ++ (show n) ++ ") {\n" ++ (tabString $ show e1) ++ "\n}"
  show (TryCatch e1 cps e2 _) = "try {\n" ++ (tabString $ show e1) ++ "\n} catch ("++ (show cps) ++ ") {\n" ++ (tabString $ show e2) ++ "\n}"

  show (ExprType t) = show t
  show (TypeConstraint tc g) = (show tc) ++ " " ++ g
  show (LocalAssign (DefVar s (ExprType (FuncType ps out cs)) _)) = "type " ++ s ++ " (" ++ (show ps) ++ ") -> " ++ (show out) ++ 
      (if cs == Nothing then "" else " ~ (" ++ (show $ fromJust cs) ++ ")")
  show (LocalAssign (DefVar s (Func ps e1) _)) = "func " ++ s ++ " (" ++ (show ps) ++ ") = " ++ (show e1)
  show (FuncCall s ps _)  = s ++ "(" ++ (show ps) ++ ")"
  show (Return e1 _) = "return (" ++ (show e1) ++ ")"
  show (FuncBlock e1 _) = "{\n" ++ (tabString $ show e1) ++ "\n}"
  show (BuiltInFunc s _) = s

  show (Literal l) = show l
  show (Value v) = show v
  show (Op op _) = show op
  show (DefPointerVar s e) = "*" ++ s ++ " = " ++ (show e)
  show (PointerExpr e) = "*(" ++ (show e) ++ ")"
  show (AddressExpr e _) = "&(" ++ (show e) ++ ")"
  show (BooleanNotExpr e _) = "!(" ++ (show e) ++ ")"
  show (GlobalVar s _) = "global " ++ s
  show (LocalAssign (DefVar s e1 _)) = s ++ " = " ++ (show e1)
  show (GlobalAssign (DefVar s e1 _)) = "global " ++ s ++ " = " ++ (show e1)
  show (Var s _) = s
  show (Seq e1 e2) = (show e1) ++ ";\n" ++ (show e2)

type Pos = (Int, Int)

printStdErr :: String -> IO ()
printStdErr s = do
    hPutStrLn stderr s
    return ()

tabString :: String -> String
tabString s = "\t" ++ (foldr (\x acc -> if x == '\n' then "\n\t"++acc else x:acc) "" s)

printPos :: Pos -> String
printPos (ln, cn) = " on line " ++ (show ln) ++ ", column " ++ (show cn)
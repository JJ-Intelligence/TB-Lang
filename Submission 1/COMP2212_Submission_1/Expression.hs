-- This file contains all of the main data types used in the language, as well as some common methods frequently used.
module Expression where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Lazy as MapL
import System.IO (hPutStrLn, stderr)
import Data.Maybe (fromJust)

-- ** CESK data types **

type State = (Expr, Environment, Store, Address, CallStack, Kon) -- The current state/configuration of the modfiied CESK machine.
type Environment = Map.Map String Address -- Used to map variable names to an address in the Store.
type Store = MapL.IntMap ExprValue -- Used to map addresses to evaluated expressions.
type CallStack = [Expr] -- Maintains a list of function calls.
type Address = Int -- Int address used in the Enivronment and Store.
type Kon = [Frame] -- Used to store Frames, containing semi-evaluated computations.

-- Frame are structures put onto the Kontinuation.
-- They contain semi-evaluated computations, allowing the finite state machine to determine its next transition.
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
           | ThrownException ExprValue CallStack
           | CatchFrame [ExprValue] Expr Environment CallStack
           | Done 
           deriving (Eq, Show)

-- Frames used for binary operations.
data BinOpFrame = BinCompOp ExprComp Expr Environment
                | BinSeqOp Expr
                | BinConsOp Expr Environment
                | BinMathOp ExprMath Expr Environment
                | BinFuncCallFrame String Expr Environment
                deriving (Eq, Show)

-- Frames used for ternary operations.
data TerOpFrame = TerIfOp Expr (Maybe ExprElif)
                | TerWhileOp Expr Expr
                | TerForInit Expr Expr Expr Expr
                | TerForOp Expr Expr Expr
                deriving (Eq, Show)


-- ** Expression data types **

-- Types are used to specify the type of an expression.
data Type = TFunc [Type] Type [(String, TypeClass)]
          | TInt 
          | TBool 
          | TEmpty 
          | TNone
          | TIterable Type
          | TList Type
          | TStream -- (note - Streams are always of type)
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

-- An expression generated from Tokens in the parser.
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

-- Optional elif/else parts of an If statement.
data ExprElif = Elif Expr Expr (Maybe ExprElif) Pos
              | Else Expr
              deriving (Eq, Ord)

instance Show ExprElif where
  show (Elif c e1 Nothing _) = " elif (" ++ (show c) ++ ") {\n" ++ (tabString $ show e1) ++ "\n}" 
  show (Elif c e1 (Just e2) _) = " elif (" ++ (show c) ++ ") {\n" ++ (tabString $ show e1) ++ "\n}" ++ (show e2) 
  show (Else e1) = " else {\n" ++ (tabString $ show e1) ++ "\n}"

-- Literals created by the parser.
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

-- Exceptions.
data Exception = EmptyListException
               | IndexOutOfBoundException
               | StreamOutOfInputException
               | InvalidParameterException
               | InvalidInputException
               | NonExhaustivePatternException
               deriving (Eq, Show)

-- Evaluated expressions. These can be stored in the Store.
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

-- Binary operations.
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

-- Parameters in a function call.
data Parameters = FuncParam Expr Parameters
                | FuncParamEnd
                deriving (Eq, Ord)

instance Show Parameters where
  show (FuncParam e1 FuncParamEnd) = show e1
  show (FuncParam e1 e2) = (show e1) ++ ", " ++ (show e2)
  show (FuncParamEnd) = ""

-- Variable assignment.
data Assignment = DefVar String Expr Pos
                deriving (Eq, Show, Ord)

data TypeClass = CEq
               | CItr
               | COrd
               | CPrintable
               deriving (Eq, Ord)

instance Show TypeClass where
    show CEq = "Eq"
    show CItr = "Itr"
    show COrd = "Ord"
    show CPrintable = "Printable"

type Pos = (Int, Int) -- The position (row/column) of something in the inputted file.

-- Prints some text to stderr.
printStdErr :: String -> IO ()
printStdErr s = hPutStrLn stderr s

-- Adds tabs to a shown expression (used for displaying statements/loops/functions).
tabString :: String -> String
tabString s = "\t" ++ (foldr (\x acc -> if x == '\n' then "\n\t"++acc else x:acc) "" s)

-- Gets a position in String form.
printPos :: Pos -> String
printPos (ln, cn) = " on line " ++ (show ln) ++ ", column " ++ (show cn)

-- Gets a list of tuples containing inbuilt variable/function names, types, and values.
getInBuiltVars :: [(String, Type, ExprValue)]
getInBuiltVars = [("tail", TFunc [TList $ TGeneric "a"] (TList $ TGeneric "a") [], VFunc 
                    (TFunc [TList $ TGeneric "a"] (TList $ TGeneric "a") []) 
                    [([VVar "xs"], BuiltInFunc "tail" [Var "xs" (0,0)])]),
                
                ("head", TFunc [TList $ TGeneric "a"] (TGeneric "a") [], VFunc 
                    (TFunc [TList $ TGeneric "a"] (TGeneric "a") []) 
                    [([VVar "xs"], BuiltInFunc "head" [Var "xs" (0,0)])]),
                
                ("drop", TFunc [TInt, TList $ TGeneric "a"] (TList $ TGeneric "a") [], VFunc 
                    (TFunc [TInt, TList $ TGeneric "a"] (TList $ TGeneric "a") []) 
                    [([VVar "n", VVar "xs"], BuiltInFunc "drop" [Var "n" (0,0), Var "xs" (0,0)])]),
                
                ("take", TFunc [TInt, TList $ TGeneric "a"] (TList $ TGeneric "a") [], VFunc 
                    (TFunc [TInt, TList $ TGeneric "a"] (TList $ TGeneric "a") []) 
                    [([VVar "n", VVar "xs"], BuiltInFunc "take" [Var "n" (0,0), Var "xs" (0,0)])]),
                
                ("length", TFunc [TList $ TGeneric "a"] (TInt) [], VFunc 
                    (TFunc [TList $ TGeneric "a"] (TInt) []) 
                    [([VVar "xs"], BuiltInFunc "length" [Var "xs" (0,0)])]),
                
                ("get", TFunc [TInt, TList $ TGeneric "a"] (TGeneric "a") [], VFunc 
                    (TFunc [TInt, TList $ TGeneric "a"] (TGeneric "a") []) 
                    [([VVar "n", VVar "xs"], BuiltInFunc "get" [Var "n" (0,0), Var "xs" (0,0)])]),
                
                ("out", TFunc [TGeneric "a"] (TNone) [("a", CPrintable)], VFunc 
                    (TFunc [TGeneric "a"] (TNone) [("a", CPrintable)])
                    [([VVar "v"], BuiltInFunc "out" [Var "v"(0,0)])]),
                
                ("in", TFunc [TInt] (TRef $ TStream) [], VFunc 
                    (TFunc [TInt] (TRef $ TStream) []) 
                    [([VVar "v"], BuiltInFunc "in" [Var "v"(0,0)])]),
                
                ("setIn", TFunc [TList $ TInt] (TNone) [], VFunc 
                    (TFunc [TList $ TInt] (TNone) []) 
                    [([VVar "xs"], BuiltInFunc "setIn" [Var "xs"(0,0)])]),
                
                ("pop", TFunc [TRef $ TIterable $ TGeneric "a"] (TGeneric "a") [], VFunc 
                    (TFunc [TRef $ TIterable $ TGeneric "a"] (TGeneric "a") []) 
                    [([VPointer "xs"], BuiltInFunc "pop" [Var "xs"(0,0)])]),
                
                ("popN", TFunc [TInt, TRef $ TIterable $ TGeneric "a"] (TList $ TGeneric "a") [], VFunc 
                    (TFunc [TInt, TRef $ TIterable $ TGeneric "a"] (TList $ TGeneric "a") []) 
                    [([VVar "n", VPointer "xs"], BuiltInFunc "popN" [Var "n"(0,0), Var "xs"(0,0)])]),
                
                ("peek", TFunc [TRef $ TIterable $ TGeneric "a"] (TGeneric "a") [], VFunc 
                    (TFunc [TRef $ TIterable $ TGeneric "a"] (TGeneric "a") []) 
                    [([VPointer "xs"], BuiltInFunc "peek" [Var "xs"(0,0)])]),
                
                ("peekN", TFunc [TInt, TRef $ TIterable $ TGeneric "a"] (TList $ TGeneric "a") [], VFunc 
                    (TFunc [TInt, TRef $ TIterable $ TGeneric "a"] (TList $ TGeneric "a") []) 
                    [([VVar "n", VPointer "xs"], BuiltInFunc "peekN" [Var "n"(0,0), Var "xs"(0,0)])]),

                ("isEmpty", TFunc [TRef $ TIterable $ TGeneric "a"] (TBool) [], VFunc 
                    (TFunc [TRef $ TIterable $ TGeneric "a"] (TBool) []) 
                    [([VPointer "xs"], BuiltInFunc "isEmpty" [Var "xs"(0,0)])]),

                ("hasElems", TFunc [TInt, TRef $ TIterable $ TGeneric "a"] (TBool) [], VFunc
                    (TFunc [TInt, TRef $ TIterable $ TGeneric "a"] (TBool) [])
                    [([VVar "n", VPointer "xs"], BuiltInFunc "hasElems" [Var "n"(0,0), Var "xs"(0,0)])]),

                ("throw", TFunc [TException] (TNone) [], VFunc
                    (TFunc [TException] (TNone) [])
                    [([VVar "e"], BuiltInFunc "throw" [Var "e"(0,0)])]),

                ("EmptyListException", TException, VException EmptyListException),
                ("IndexOutOfBoundException", TException, VException IndexOutOfBoundException),
                ("StreamOutOfInputException", TException, VException StreamOutOfInputException),
                ("InvalidParameterException", TException, VException InvalidParameterException),
                ("NonExhaustivePatternException", TException, VException NonExhaustivePatternException),
                ("InvalidInputException", TException, VException InvalidInputException)]
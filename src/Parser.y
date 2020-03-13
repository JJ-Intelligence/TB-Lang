{ 
module Parser where 
import Lexer
}

%name parse
%tokentype { Token } 
%error { parseError }
%token
    if     { TokenIf _ }
    elif   { TokenElif _ }
    else   { TokenElse _ }

    func   { TokenFuncDef _ }
    return { TokenReturn _ }

    ';'    { TokenSeq _ }
    '('    { TokenOpenParen _ }
    ')'    { TokenCloseParen _ }
    '{'    { TokenOpenCurly _ }
    '}'    { TokenCloseCurly _ }
    '['    { TokenOpenSquare _ }
    ']'    { TokenCloseSquare _ }
    ','    { TokenComma _ }

    '+'    { TokenPlus _ }
    '-'    { TokenMinus _ }
    '*'    { TokenMultiply _ }
    '/'    { TokenDivide _ }
    '^'    { TokenExponent _ }
    '%'    { TokenModulus _ }
    '=='   { TokenDoubleEquals _ }
    and    { TokenAnd _ }
    or     { TokenOr _ }
    '<'    { TokenLessThan _ }
    '>'    { TokenGreaterThan _ }
    '='    { TokenEquals _ }
    ':'    { TokenCons _ }

    int    { TokenInt $$ _ }
    bool   { TokenBool $$ _ }

    var    { TokenVar $$ _ }

%right ';'
%left '='
%right ':'
%left or
%left and
%left '==' '<' '>'
%left '+' '-'
%left '*' '/' '%'
%left NEG
%right '^'
%%

E : E ';' E                         { Seq $1 $3 }
  | E ';'                           { $1 }
  | if '(' E ')' B EElif            { If $3 $5 (Just $6) }
  | if '(' E ')' B                  { If $3 $5 Nothing }
  | func var '(' P ')' '=' E        { DefVar $2 (Func $4 $7) }
  | return E                        { Return $2 }
  | var '=' E                       { DefVar $1 $3 }
  | var                             { Var $1 }
  | B                               { $1 }
  | O                               { $1 }
  | C                               { $1 }
  | L                               { $1 }

P : E ',' P                         { FuncParam $1 $3 }
  | E                               { FuncParamEnd }

-- Elif part of an If statement.
EElif : elif '(' E ')' B EElif      { Elif $3 $5 (Just $6) }
      | elif '(' E ')' B            { Elif $3 $5 Nothing }
      | else B                      { Else $2 }

-- Function block.
B : '{' E '}'                       { $2 }
  | '(' E ')'                       { $2 }

-- Binary operations.
O : E '==' E                        { Op (CompOp Equality $1 $3) }
  | E ':' E                         { Op (Cons $1 $3) }
  | E '+' E                         { Op (MathOp Plus $1 $3) }
  | E '-' E                         { Op (MathOp Min $1 $3) }
  | E '*' E                         { Op (MathOp Mult $1 $3) }
  | E '/' E                         { Op (MathOp Div $1 $3) }
  | E '^' E                         { Op (MathOp Exp $1 $3) }
  | E '%' E                         { Op (MathOp Mod $1 $3) }
  | E and E                         { Op (CompOp And $1 $3) }
  | E or E                          { Op (CompOp Or $1 $3) }
  | E '<' E                         { Op (CompOp LessThan $1 $3) }
  | E '>' E                         { Op (CompOp GreaterThan $1 $3) }

-- List operations.
C : '[' C2 ']'                      { $2 }
  | '[' ']'                         { Literal Empty }

C2 : E ',' C2                       { Op (Cons $1 $3) }
   | E                              { Op (Cons $1 (Literal Empty)) }

-- Literals.
L : '-'int %prec NEG                { Literal (EInt (-$2)) }
  | int                             { Literal (EInt $1) }
  | bool                            { Literal (EBool $1) }

{

parseError :: [Token] -> a
parseError [] = error "ERROR: End of Tokens parse error"
parseError (x:xs) = error ("ERROR: Parse error at line " ++ (show l) ++ ", column " ++ (show c) ++ ", parsing \'"++ (show x) ++"\'.")
        where (l,c) = tokenPosn x

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
                 deriving (Eq)

instance Show ExprLiteral where
  show (EInt n) = show n
  show (EBool b) = show b
  show Empty = "[]"

data ExprValue = VInt Int
               | VBool Bool
               | VList [ExprValue]
               | VNone
               deriving (Eq)

instance Show ExprValue where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VList []) = "[]"
  show (VList xs) = filter (/='"') ("[" ++ (helper (map (show) xs)) ++ "]")
    where helper [] = ""
          helper [y] = show y
          helper (x:y:xs) = (show x) ++ "," ++ (helper (y:xs))
  show VNone = "null"

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

data Parameter = FuncParam Expr Parameter
               | FuncParamEnd
               deriving (Eq)

instance Show Parameter where
  show (FuncParam e1 FuncParamEnd) = show e1
  show (FuncParam e1 e2) = (show e1) ++ ", " ++ (show e2)
  show (FuncParamEnd) = ""

data Expr = If Expr Expr (Maybe ExprElif)
          | Func Parameter Expr
          | Return Expr
          | Literal ExprLiteral
          | Value ExprValue
          | Op BinOp
          | DefVar String Expr
          | Var String
          | Seq Expr Expr
          deriving (Eq)

instance Show Expr where
  show (If c e1 Nothing) = "if (" ++ (show c) ++ ") {\n" ++ (show e1) ++ "\n}\n"
  show (If c e1 (Just e2)) = "if (" ++ (show c) ++ ") {\n" ++ (show e1) ++ "\n} " ++ (show e2)
  show (DefVar s (Func ps e1)) = "func " ++ s ++ " (" ++ (show ps) ++ ") = {\n" ++ (show e1) ++ "}\n" 
  show (Return e1) = "return " ++ (show e1)
  show (Literal l) = show l
  show (Value v) = show v
  show (Op op) = show op
  show (DefVar s e1) = s ++ " = " ++ (show e1)
  show (Var s) = s
  show (Seq e1 e2) = (show e1) ++ ";\n" ++ (show e2)

}

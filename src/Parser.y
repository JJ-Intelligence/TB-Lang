{ 
module Parser where 
import Lexer
import Expression
}

%name parse
%tokentype { Token } 
%error { parseError }
%token
    if     { TokenIf _ }
    elif   { TokenElif _ }
    else   { TokenElse _ }

    while  { TokenWhile _ }

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

    '*'    { TokenStar _ }
    '&'    { TokenAddress _ }

    int    { TokenInt $$ _ }
    bool   { TokenBool $$ _ }
    none   { TokenNone _ }

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
%right POINT '&'
%%

E : E ';' E                         { Seq $1 $3 }
  | E ';'                           { $1 }
  | while '(' E ')' B               { While $3 $5 }
  | if '(' E ')' B EElif            { If $3 $5 (Just $6) }
  | if '(' E ')' B                  { If $3 $5 Nothing }
  | func var '(' P ')' '=' E        { DefVar $2 (Func $4 $7) }
  | func var '(' ')' '=' E          { DefVar $2 (Func FuncParamEnd $6) }
  | return '(' E ')'                { Return $3 }
  | return '(' ')'                  { Return (Literal ENone) }
  | var '(' P ')'                   { FuncCall $1 $3 }
  | var '('')'                      { FuncCall $1 FuncParamEnd }
  | V                               { $1 }
  | B                               { $1 }
  | O                               { $1 }
  | C                               { $1 }
  | L                               { $1 }

V : '*'var '=' E                    { DefPointerVar $2 $4 }
  | '*'var                          { PointerVar $2 }
  | '&'var                          { AddressVar $2 }
  | var '=' E                       { DefVar $1 $3 }
  | var                             { Var $1 }

P : E ',' P                         { FuncParam $1 $3 }
  | E                               { FuncParam $1 FuncParamEnd }

-- Elif part of an If statement.
EElif : elif '(' E ')' B EElif      { Elif $3 $5 (Just $6) }
      | elif '(' E ')' B            { Elif $3 $5 Nothing }
      | else B                      { Else $2 }

-- Function block.
B : '{' E '}'                       { FuncBlock $2 }
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
  | none                            { Literal ENone }

{

parseError :: [Token] -> a
parseError [] = error "ERROR: End of Tokens parse error"
parseError (x:xs) = error ("ERROR: Parse error at line " ++ (show l) ++ ", column " ++ (show c) ++ ", parsing \'"++ (show x) ++"\'.")
        where (l,c) = tokenPosn x

}

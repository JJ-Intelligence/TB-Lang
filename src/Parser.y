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
    for    { TokenFor _ }

    '->'   { TokenReturnTypeArrow _ }
    type   { TokenFuncType _ }
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

    '++'    { TokenPlusPlus _ }
    '--'    { TokenMinusMinus _ }
    '+='    { TokenPlusAssignment _ }
    '-='    { TokenMinusAssignment _ }
    '*='    { TokenMultiplyAssignment _ }
    '/='    { TokenDivideAssignment _ }
    '^='    { TokenExponentAssignment _ }
    '&='    { TokenAndAssignment _ }
    '|='    { TokenOrAssignment _ }

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

    tInt   { TokenTypeInt _ }
    tBool  { TokenTypeBool _ }
    tNone  { TokenTypeNone _ }
    tStream { TokenTypeStream _ }

    cEq    {TokenTypeConstraintEq _ }
    cItr   {TokenTypeConstraintItr _ }
    cOrd   {TokenTypeConstraintOrd _ }
    '~'    {TokenTypeConstraintTwiddle _ }

    int    { TokenInt $$ _ }
    bool   { TokenBool $$ _ }
    none   { TokenNone _ }

    global { TokenGlobal _ }
    var    { TokenVar $$ _ }

%right ';'
%right '->' -- unsure about this
%left '~' -- unsure about this
%left '=' '+=' '-=' '*=' '/=' '^=' '&=' '|='
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

E : E ';' E                                 { Seq $1 $3 }
  | E ';'                                   { $1 }
  | while '(' E ')' '{' E '}'               { While $3 $6 }
  | if '(' E ')' '{' E '}' EElif            { If $3 $6 (Just $8) }
  | if '(' E ')' '{' E '}'                  { If $3 $6 Nothing }
  | for '(' E ';' E ';' E ')' '{' E '}'     { For $3 $5 $7 $10 }
  | type var FT                             { LocalAssign $ DefVar $2 $3 }
  | func var '(' P ')' '=' E                { LocalAssign $ DefVar $2 (Func $4 $7) }
  | func var '(' ')' '=' E                  { LocalAssign $ DefVar $2 (Func FuncParamEnd $6) }
  | return '(' E ')'                        { Return $3 }
  | return '(' ')'                          { Return (Literal ENone) }
  | var '(' P ')'                           { FuncCall $1 $3 }
  | var '('')'                              { FuncCall $1 FuncParamEnd }
  -- | var '++'                             { FuncBlock (Seq (DefVar $1 (Op (MathOp Plus (Var $1) (Literal $ EInt 1)))) (Return (Op (MathOp Min (Var $1) (Literal $ EInt 1))))) }
  -- | var '--'                             { FuncBlock (Seq (DefVar $1 (Op (MathOp Min (Var $1) (Literal $ EInt 1)))) (Return (Op (MathOp Plus (Var $1) (Literal $ EInt 1))))) }
  -- | '++' var                             { DefVar $2 (Op (MathOp Plus (Var $2) (Literal $ EInt 1))) }
  -- | '--' var                             { DefVar $2 (Op (MathOp Min (Var $2) (Literal $ EInt 1))) }
  | '&'E                                    { AddressExpr $2 }
  | '*'E %prec POINT                        { PointerExpr $2 }
  | FT                                      { $1 }
  | V                                       { $1 }
  | B                                       { $1 }
  | O                                       { $1 }
  | C                                       { $1 }
  | L                                       { $1 }

FT : '(' FTP ')' '->' FT               { FuncType $2 $5 Nothing }
   | '(' FTP ')' '->' TL               { FuncType $2 (ExprType $5) Nothing }
   | '(' FTP ')' '->' FT '~' '(' PC ')'{ FuncType $2 $5 (Just $8) }
   | '(' FTP ')' '->' TL '~' '(' PC ')'{ FuncType $2 (ExprType $5) (Just $8) }

FTP : FT ',' FTP                       { FuncParam $1 $3 }
    | FT                               { FuncParam $1 FuncParamEnd }
    | TL ',' FTP                       { FuncParam (ExprType $1) $3 }
    | TL                               { FuncParam (ExprType $1) FuncParamEnd }

TL : '('TL')'                       { $2 }
   | '[' ']'                        { TList TEmpty }
   -- | '[' var ']'                    { TList $ TGeneric $2 }
   | '[' TL ']'                     { TList $2 }
   -- | var'*'                         { TRef $ TGeneric $1 }
   | TL'*'                          { TRef $1 }
   | tInt                           { TInt }
   | tBool                          { TBool }
   | tNone                          { TNone }
   | tStream                        { TStream }
   -- | cItr var                       { TIterable $ TGeneric $2 }
   | cItr TL                        { TIterable $2 }
   | var                            { TGeneric $1 }

PC : TC ',' PC                        { FuncParam $1 $3 }
   | TC                               { FuncParam $1 FuncParamEnd }

TC : cEq var                        { TypeConstraint CEq $2 }
   | cItr var                       { TypeConstraint CItr $2 }
   | cOrd var                       { TypeConstraint COrd $2 }

V : global GV                       { GlobalAssign $2 }
  | GV                              { LocalAssign $1 }
  | '*'var '=' E                    { DefPointerVar $2 $4 }
  | global var                      { GlobalVar $2 }
  | var                             { Var $1 }

GV : var '+=' E                      { DefVar $1 (Op (MathOp Plus (Var $1) $3)) }
   | var '-=' E                      { DefVar $1 (Op (MathOp Min (Var $1) $3)) }
   | var '*=' E                      { DefVar $1 (Op (MathOp Mult (Var $1) $3)) }
   | var '/=' E                      { DefVar $1 (Op (MathOp Div (Var $1) $3)) }
   | var '^=' E                      { DefVar $1 (Op (MathOp Exp (Var $1) $3)) }
   | var '&=' E                      { DefVar $1 (Op (CompOp And (Var $1) $3)) }
   | var '|=' E                      { DefVar $1 (Op (CompOp Or (Var $1) $3)) }
   | var '=' E                       { DefVar $1 $3 }

P : E ',' P                         { FuncParam $1 $3 }
  | E                               { FuncParam $1 FuncParamEnd }

-- Elif part of an If statement.
EElif : elif '(' E ')' B EElif      { Elif $3 $5 (Just $6) }
      | elif '(' E ')' B            { Elif $3 $5 Nothing }
      | else B                      { Else $2 }

-- Function block.
B : '{' E '}'                       { FuncBlock $2 }
  | '{' '}'                         { Literal ENone }
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

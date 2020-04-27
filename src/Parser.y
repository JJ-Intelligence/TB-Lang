{ 
module Parser where 
import Lexer
import Expression
}

%name parse
%tokentype { Token } 
%error { parseError }
%token
    if        { TokenIf $$ }
    elif      { TokenElif $$ }
    else      { TokenElse _ }

    while     { TokenWhile $$ }
    for       { TokenFor $$ }

    '->'      { TokenReturnTypeArrow _ }
    type      { TokenFuncType $$ }
    func      { TokenFuncDef $$ }
    return    { TokenReturn $$ }

    try       { TokenTry _ }
    catch     { TokenCatch $$ }

    ';'       { TokenSeq _ }
    '('       { TokenOpenParen _ }
    ')'       { TokenCloseParen _ }
    '{'       { TokenOpenCurly $$ }
    '}'       { TokenCloseCurly _ }
    '['       { TokenOpenSquare $$ }
    ']'       { TokenCloseSquare _ }
    ','       { TokenComma $$ }

    '++'      { TokenPlusPlus $$ }
    '--'      { TokenMinusMinus $$ }
    '+='      { TokenPlusAssignment $$ }
    '-='      { TokenMinusAssignment $$ }
    '*='      { TokenMultiplyAssignment $$ }
    '/='      { TokenDivideAssignment $$ }
    '^='      { TokenExponentAssignment $$ }
    '&='      { TokenAndAssignment $$ }
    '|='      { TokenOrAssignment $$ }

    '+'       { TokenPlus $$ }
    '-'       { TokenMinus $$ }
    '/'       { TokenDivide $$ }
    '^'       { TokenExponent $$ }
    '%'       { TokenModulus $$ }
    '=='      { TokenDoubleEquals $$ }
    '>='      { TokenGreaterThanEquals $$ }
    '<='      { TokenLessThanEquals $$ }
    '!='      { TokenNotEquals $$ }
    and       { TokenAnd $$ }
    or        { TokenOr $$ }
    '<'       { TokenLessThan $$ }
    '>'       { TokenGreaterThan $$ }
    ':'       { TokenCons $$ }
    '!'       { TokenNot $$ }
    '='       { TokenEquals _ }

    '*'       { TokenStar $$ }
    '&'       { TokenAddress $$ }

    tInt      { TokenTypeInt _ }
    tBool     { TokenTypeBool _ }
    tNone     { TokenTypeNone _ }
    tStream   { TokenTypeStream _ }

    cEq       {TokenTypeConstraintEq _ }
    cItr      {TokenTypeConstraintItr _ }
    cOrd      {TokenTypeConstraintOrd _ }
    '~'       {TokenTypeConstraintTwiddle _ }

    int       { TokenInt $$ _ }
    bool      { TokenBool $$ _ }
    none      { TokenNone _ }

    global    { TokenGlobal _ }
    var       { TokenVar $$ }

%right ';'
%right '->'
%left '~'
%left '=' '+=' '-=' '*=' '/=' '^=' '&=' '|='
%right ':'
%left or
%left and
%left '==' '!=' '<' '>' '>=' '<='
%left '+' '-'
%left '*' '/' '%'
%left NEG
%right '^' 
%right POINT '&' '!'

%%

E : E ';' E                                 { Seq $1 $3 }
  | E ';'                                   { $1 }
  | while '(' E ')' '{' E '}'               { While $3 $6 $1 }
  | if '(' E ')' '{' E '}' EElif            { If $3 $6 (Just $8) $1 }
  | if '(' E ')' '{' E '}'                  { If $3 $6 Nothing $1 }
  | for '(' E ';' E ';' E ')' '{' E '}'     { For $3 $5 $7 $10 $1 }
  | type var FT                             { LocalAssign $ DefVar (fst $2) (ExprType $3) $1 }
  | func var '(' P ')' '=' E                { LocalAssign $ DefVar (fst $2) (Func $4 $7) $1 }
  | func var '(' ')' '=' E                  { LocalAssign $ DefVar (fst $2) (Func FuncParamEnd $6) $1 }
  | return '(' E ')'                        { Return $3 $1 }
  | return '(' ')'                          { Return (Literal ENone) $1 }
  | var '(' P ')'                           { FuncCall (fst $1) $3 (snd $1) }
  | var '('')'                              { FuncCall (fst $1) FuncParamEnd (snd $1) }
  | var '++'                                { FuncBlock (Seq (LocalAssign $ DefVar (fst $1) (Op (MathOp Plus (Var (fst $1) (snd $1)) (Literal $ EInt 1)) (snd $1)) (snd $1)) (Return (Op (MathOp Min (Var (fst $1) (snd $1)) (Literal $ EInt 1)) (snd $1)) (snd $1))) (snd $1) }
  | var '--'                                { FuncBlock (Seq (LocalAssign $ DefVar (fst $1) (Op (MathOp Min (Var (fst $1) (snd $1)) (Literal $ EInt 1)) (snd $1)) (snd $1)) (Return (Op (MathOp Plus (Var (fst $1) (snd $1)) (Literal $ EInt 1)) (snd $1)) (snd $1))) (snd $1) } 
  | '++' var                                { LocalAssign $ DefVar (fst $2) (Op (MathOp Plus (Var (fst $2) (snd $2)) (Literal $ EInt 1)) (snd $2)) (snd $2) }
  | '--' var                                { LocalAssign $ DefVar (fst $2) (Op (MathOp Min (Var (fst $2) (snd $2)) (Literal $ EInt 1)) (snd $2)) (snd $2) }
  | '&' E                                   { AddressExpr $2 $1 }
  | '*' var '=' E %prec POINT               { DefPointerVar (fst $2) $4 }
  | '*' var %prec POINT                     { PointerExpr (Var (fst $2) (snd $2)) }
  | '*' '(' E ')' %prec POINT               { PointerExpr $3 } -- !!6 reduce/reduce conflicts due to this!!
  | '!' E                                   { BooleanNotExpr $2 $1 }
  | try '{' E '}' catch '(' P ')' '{' E '}' { TryCatch $3 $7 $10 $5 }
  | V                                       { $1 }
  | B                                       { $1 }
  | O                                       { $1 }
  | C                                       { $1 }
  | L                                       { $1 }

FT : '(' FTP ')' '->' TL                    { FuncType $2 (ExprType $5) Nothing }
   | '(' FTP ')' '->' TL '~' '(' PC ')'     { FuncType $2 (ExprType $5) (Just $8) }

FTP : TL ',' FTP                            { FuncParam (ExprType $1) $3 }
    | TL                                    { FuncParam (ExprType $1) FuncParamEnd }

TL : FT                                     { $1 }
   | '[' ']'                                { TList TEmpty }
   | '[' TL ']'                             { TList $2 }
   | '*'TL                                  { TRef $2 }
   | tInt                                   { TInt }
   | tBool                                  { TBool }
   | tNone                                  { TNone }
   | tStream                                { TStream }
   | cItr TL                                { TIterable $2 }
   | var                                    { TGeneric (fst $1) }

PC : TC ',' PC                              { FuncParam $1 $3 }
   | TC                                     { FuncParam $1 FuncParamEnd }

TC : cEq var                                { TypeConstraint CEq (fst $2) }
   | cItr var                               { TypeConstraint CItr (fst $2) }
   | cOrd var                               { TypeConstraint COrd (fst $2) }

V : global GV                               { GlobalAssign $2 }
  | GV                                      { LocalAssign $1 }
  | global var                              { GlobalVar (fst $2) (snd $2) }
  | var                                     { Var (fst $1) (snd $1) }

GV : var '+=' E                             { DefVar (fst $1) (Op (MathOp Plus (Var (fst $1) (snd $1)) $3) $2) $2 }
   | var '-=' E                             { DefVar (fst $1) (Op (MathOp Min (Var (fst $1) (snd $1)) $3) $2) $2 }
   | var '*=' E                             { DefVar (fst $1) (Op (MathOp Mult (Var (fst $1) (snd $1)) $3) $2) $2 }
   | var '/=' E                             { DefVar (fst $1) (Op (MathOp Div (Var (fst $1) (snd $1)) $3) $2) $2 }
   | var '^=' E                             { DefVar (fst $1) (Op (MathOp Exp (Var (fst $1) (snd $1)) $3) $2) $2 }
   | var '&=' E                             { DefVar (fst $1) (Op (CompOp And (Var (fst $1) (snd $1)) $3) $2) $2 }
   | var '|=' E                             { DefVar (fst $1) (Op (CompOp Or (Var (fst $1) (snd $1)) $3) $2) $2 }
   | var '=' E                              { DefVar (fst $1) $3 (snd $1) }

P : E ',' P                                 { FuncParam $1 $3 }
  | E                                       { FuncParam $1 FuncParamEnd }

-- Elif part of an If statement.
EElif : elif '(' E ')' '{' E '}' EElif      { Elif $3 $6 (Just $8) $1 }
      | elif '(' E ')' '{' E '}'            { Elif $3 $6 Nothing $1 }
      | else '{' E '}'                      { Else $3 }

-- Function block.
B : '{' E '}'                               { FuncBlock $2 $1 }
  | '{' '}'                                 { Literal ENone }
  | '(' E ')'                               { $2 }

-- Binary operations.
O : E '==' E                                { Op (CompOp Equality $1 $3) $2 }
  | E '!=' E                                { Op (CompOp NotEquals $1 $3) $2 }
  | E '>=' E                                { Op (CompOp Or (Op (CompOp GreaterThan $1 $3) $2) (Op (CompOp Equality $1 $3) $2)) $2 }
  | E '<=' E                                { Op (CompOp Or (Op (CompOp LessThan $1 $3) $2) (Op (CompOp Equality $1 $3) $2)) $2 }
  | E ':' E                                 { Op (Cons $1 $3) $2 }
  | E '+' E                                 { Op (MathOp Plus $1 $3) $2 }
  | E '-' E                                 { Op (MathOp Min $1 $3) $2 }
  | E '*' E                                 { Op (MathOp Mult $1 $3) $2 }
  | E '/' E                                 { Op (MathOp Div $1 $3) $2 }
  | E '^' E                                 { Op (MathOp Exp $1 $3) $2 }
  | E '%' E                                 { Op (MathOp Mod $1 $3) $2 }
  | E and E                                 { Op (CompOp And $1 $3) $2 }
  | E or E                                  { Op (CompOp Or $1 $3) $2 }
  | E '<' E                                 { Op (CompOp LessThan $1 $3) $2 }
  | E '>' E                                 { Op (CompOp GreaterThan $1 $3) $2 }

-- List operations.
C : '[' C2 ']'                              { $2 }
  | '[' E ']'                               { (Op (Cons $2 (Literal Empty)) $1) }
  | '[' ']'                                 { Literal Empty }

C2 : E ',' C2                               { Op (Cons $1 $3) $2 }
   | E ',' E                                { Op (Cons $1 (Op (Cons $3 (Literal Empty)) $2)) $2 }
   -- | E                                      { Op (Cons $1 (Literal Empty)) }

-- Literals.
L : '-'int %prec NEG                        { Literal (EInt (-$2)) }
  | int                                     { Literal (EInt $1) }
  | bool                                    { Literal (EBool $1) }
  | none                                    { Literal ENone }

{

parseError :: [Token] -> a
parseError [] = error "Parse ERROR: End of Tokens parse error"
parseError (x:xs) = error ("Parse ERROR: Trying to parse \'" ++ (if last s == ' ' then s else init s) ++ "\', on line " ++ (show l) ++ ", column " ++ (show c))
        where (l,c) = tokenPos x
              s = show x

}

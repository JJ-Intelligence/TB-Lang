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

    ';'    { TokenSeq _ }
    '('    { TokenOpenParen _ }
    ')'    { TokenCloseParen _ }
    '{'    { TokenOpenCurly _ }
    '}'    { TokenCloseCurly _ }

    '=='   { TokenDoubleEquals _ }
    '='    { TokenEquals _ }

    int    { TokenInt $$ _ }
    bool   { TokenBool $$ _ }

    var    { TokenVar $$ _ }

%right ';'
%left '='
%left '=='
%%

E : E ';' E                         { Seq $1 $3 }
  | E ';'                           { $1 }
  | if '(' E ')' B EElif            { If $3 $5 (Just $6) }
  | if '(' E ')' B                  { If $3 $5 Nothing }
  | var '=' E                       { DefVar $1 $3 }
  | var                             { Var $1 }
  | B                               { $1 }
  | L                               { $1 }
  | O                               { $1 }

EElif : elif '(' E ')' B EElif      { Elif $3 $5 (Just $6) }
      | elif '(' E ')' B            { Elif $3 $5 Nothing }
      | else B                      { Else $2 }

B : '{' E '}'                       { $2 }

O : E '==' E                        { Op (CompOp Equality $1 $3) }

L : int                             { Literal (EInt $1) }
  | bool                            { Literal (EBool $1) }

{

parseError :: [Token] -> a
parseError [] = error "ERROR: End of Tokens parse error"
parseError (x:xs) = error ("ERROR: Parse error at line " ++ (show l) ++ ", column " ++ (show c) ++ ", parsing \'"++ (show x) ++"\'.")
        where (l,c) = tokenPosn x

data ExprElif = Elif Expr Expr (Maybe ExprElif)
              | Else Expr
              deriving (Show)

data ExprLiteral = EInt Int
                 | EBool Bool
                 deriving (Show)

-- Binary Operation.
data BinOp = CompOp ExprComp Expr Expr
           deriving (Show)

-- Comparison operations.
data ExprComp = Equality
              deriving (Show)

data Expr = If Expr Expr (Maybe ExprElif)
          | Literal ExprLiteral
          | Op BinOp
          | DefVar String Expr
          | Var String
          | Seq Expr Expr
          deriving (Show)

}

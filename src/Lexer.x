{ 
module Lexer where 
import Expression
}

%wrapper "posn" 
$digit = 0-9     
$alpha = [a-zA-Z]    

tokens :-
    $white+                             ;
    "--".*                              ;
    "/*".*"*/"                          ;

    if                                  { \p _ -> TokenIf (toPos p) }
    elif                                { \p _ -> TokenElif (toPos p) }
    else                                { \p _ -> TokenElse (toPos p) }
    while                               { \p _ -> TokenWhile (toPos p) }
    for                                 { \p _ -> TokenFor (toPos p) }
    func                                { \p _ -> TokenFuncDef (toPos p) }
    type                                { \p _ -> TokenFuncType (toPos p) }
    \-\>                                { \p _ -> TokenReturnTypeArrow (toPos p) }
    return                              { \p _ -> TokenReturn (toPos p) }

    try                                 { \p _ -> TokenTry (toPos p) }
    catch                               { \p _ -> TokenCatch (toPos p) }

    \;                                  { \p _ -> TokenSeq (toPos p) }
    \(                                  { \p _ -> TokenOpenParen (toPos p) }
    \)                                  { \p _ -> TokenCloseParen (toPos p) }
    \{                                  { \p _ -> TokenOpenCurly (toPos p) }
    \}                                  { \p _ -> TokenCloseCurly (toPos p) }
    \[                                  { \p _ -> TokenOpenSquare (toPos p) }
    \]                                  { \p _ -> TokenCloseSquare (toPos p) }
    \,                                  { \p _ -> TokenComma (toPos p) }

    \+\+                                { \p _ -> TokenPlusPlus (toPos p) }
    \-\-                                { \p _ -> TokenMinusMinus (toPos p) }
    \+\=                                { \p _ -> TokenPlusAssignment (toPos p) }
    \-\=                                { \p _ -> TokenMinusAssignment (toPos p) }
    \*\=                                { \p _ -> TokenMultiplyAssignment (toPos p) }
    \/\=                                { \p _ -> TokenDivideAssignment (toPos p) }
    \^\=                                { \p _ -> TokenExponentAssignment (toPos p) }
    \&\=                                { \p _ -> TokenAndAssignment (toPos p) }
    \|\=                                { \p _ -> TokenOrAssignment (toPos p) }

    \+                                  { \p _ -> TokenPlus (toPos p) }
    \-                                  { \p _ -> TokenMinus (toPos p) }
    \/                                  { \p _ -> TokenDivide (toPos p) }
    \^                                  { \p _ -> TokenExponent (toPos p) }
    \%                                  { \p _ -> TokenModulus (toPos p) }
    \=\=                                { \p _ -> TokenDoubleEquals (toPos p) }
    \>\=                                { \p _ -> TokenGreaterThanEquals (toPos p) }
    \<\=                                { \p _ -> TokenLessThanEquals (toPos p) }
    \!\=                                { \p _ -> TokenNotEquals (toPos p) }
    \&\&                                { \p _ -> TokenAnd (toPos p) }
    \|\|                                { \p _ -> TokenOr (toPos p) }
    \<                                  { \p _ -> TokenLessThan (toPos p) }
    \>                                  { \p _ -> TokenGreaterThan (toPos p) }
    \:                                  { \p _ -> TokenCons (toPos p) }
    \!                                  { \p _ -> TokenNot (toPos p) }
    \=                                  { \p _ -> TokenEquals (toPos p) }

    \*                                  { \p _ -> TokenStar (toPos p) }
    \&                                  { \p _ -> TokenAddress (toPos p) }

    Int                                 { \p _ -> TokenTypeInt (toPos p) }
    Bool                                { \p _ -> TokenTypeBool (toPos p) }
    NoneType                            { \p _ -> TokenTypeNone (toPos p) }
    Stream                              { \p _ -> TokenTypeStream (toPos p) }

    Eq                                  { \p _ -> TokenTypeConstraintEq (toPos p) }
    Itr                                 { \p _ -> TokenTypeConstraintItr (toPos p) }
    Ord                                 { \p _ -> TokenTypeConstraintOrd (toPos p) }
    \~                                  { \p _ -> TokenTypeConstraintTwiddle (toPos p) }

    $digit+                             { \p s -> TokenInt (read s) (toPos p) }
    True                                { \p _ -> TokenBool True (toPos p) }
    False                               { \p _ -> TokenBool False (toPos p) }
    None                                { \p _ -> TokenNone (toPos p) }

    global                              { \p _ -> TokenGlobal (toPos p) }
    [$alpha \_] [$alpha $digit \_]*     { \p s -> TokenVar (s, toPos p) }

{

-- Lexeme Tokens.
data Token =  TokenIf                           { pos :: Pos }
            | TokenElif                         { pos :: Pos }
            | TokenElse                         { pos :: Pos }

            | TokenWhile                        { pos :: Pos }
            | TokenFor                          { pos :: Pos }

            | TokenReturnTypeArrow              { pos :: Pos }
            | TokenFuncType                     { pos :: Pos }
            | TokenFuncDef                      { pos :: Pos }
            | TokenReturn                       { pos :: Pos }

            | TokenTry                          { pos :: Pos }
            | TokenCatch                        { pos :: Pos }
            
            | TokenSeq                          { pos :: Pos }
            | TokenOpenParen                    { pos :: Pos }
            | TokenCloseParen                   { pos :: Pos }
            | TokenOpenCurly                    { pos :: Pos }
            | TokenCloseCurly                   { pos :: Pos }
            | TokenOpenSquare                   { pos :: Pos }
            | TokenCloseSquare                  { pos :: Pos }
            | TokenComma                        { pos :: Pos }

            | TokenPlusPlus                     { pos :: Pos }
            | TokenMinusMinus                   { pos :: Pos }
            | TokenPlusAssignment               { pos :: Pos }
            | TokenMinusAssignment              { pos :: Pos }
            | TokenMultiplyAssignment           { pos :: Pos }
            | TokenDivideAssignment             { pos :: Pos }
            | TokenExponentAssignment           { pos :: Pos }
            | TokenAndAssignment                { pos :: Pos }
            | TokenOrAssignment                 { pos :: Pos }

            | TokenPlus                         { pos :: Pos }
            | TokenMinus                        { pos :: Pos }
            | TokenDivide                       { pos :: Pos }
            | TokenExponent                     { pos :: Pos }
            | TokenModulus                      { pos :: Pos }
            | TokenEquals                       { pos :: Pos }
            | TokenDoubleEquals                 { pos :: Pos }
            | TokenGreaterThanEquals            { pos :: Pos }
            | TokenLessThanEquals               { pos :: Pos }
            | TokenNotEquals                    { pos :: Pos }
            | TokenAnd                          { pos :: Pos }
            | TokenOr                           { pos :: Pos }
            | TokenLessThan                     { pos :: Pos }
            | TokenGreaterThan                  { pos :: Pos }
            | TokenCons                         { pos :: Pos }
            | TokenNot                          { pos :: Pos }

            | TokenStar                         { pos :: Pos }
            | TokenAddress                      { pos :: Pos }

            | TokenTypeInt                      { pos :: Pos }
            | TokenTypeBool                     { pos :: Pos }
            | TokenTypeNone                     { pos :: Pos }
            | TokenTypeStream                   { pos :: Pos }

            | TokenTypeConstraintEq             { pos :: Pos }
            | TokenTypeConstraintItr            { pos :: Pos }
            | TokenTypeConstraintOrd            { pos :: Pos }
            | TokenTypeConstraintTwiddle        { pos :: Pos }

            | TokenInt                          { int :: Int, pos :: Pos }
            | TokenBool                         { bool :: Bool, pos :: Pos }
            | TokenNone                         { pos :: Pos }

            | TokenGlobal                       { pos :: Pos }
            | TokenVar (String, Pos)
            deriving (Eq) 

-- Token instance of Show, used for error messages.
instance Show Token where
    show (TokenIf _) = "if "
    show (TokenElif _) = "elif "
    show (TokenElse _) = "else "

    show (TokenWhile _) = "while "

    show (TokenReturnTypeArrow _) = "-> "
    show (TokenFuncType _) = "type "
    show (TokenFuncDef _) = "func "
    show (TokenReturn _) = "return "

    show (TokenTry _) = "try "
    show (TokenCatch _) = "catch "

    show (TokenSeq _) = "; "
    show (TokenOpenParen _) = "( "
    show (TokenCloseParen _) = ") "
    show (TokenOpenCurly _) = "{ "
    show (TokenCloseCurly _) = "} "
    show (TokenOpenSquare _) = "[ "
    show (TokenCloseSquare _) = "] "
    show (TokenComma _) = ", "

    show (TokenPlus _) = "+ "
    show (TokenMinus _) = "- "
    show (TokenDivide _) = "/ "
    show (TokenExponent _) = "^ "
    show (TokenModulus _) = "% "
    show (TokenEquals _) = "= "
    show (TokenDoubleEquals _) = "== "
    show (TokenGreaterThanEquals _) = ">= "
    show (TokenLessThanEquals _) = "<= "
    show (TokenNotEquals _) = "!= "
    show (TokenAnd _) = "&& "
    show (TokenOr _) = "|| "
    show (TokenLessThan _) = "< "
    show (TokenGreaterThan _) = "> "
    show (TokenCons _) = ": "
    show (TokenNot _) = "!"

    show (TokenStar _) = "* "
    show (TokenAddress _) = "&"

    show (TokenTypeInt _) = "Int"
    show (TokenTypeBool _) = "Bool"
    show (TokenTypeNone _) = "NoneType"
    show (TokenTypeStream _) = "Stream"

    show (TokenTypeConstraintEq _) = "Eq "
    show (TokenTypeConstraintItr _) = "Itr "
    show (TokenTypeConstraintOrd _) = "Ord "
    show (TokenTypeConstraintTwiddle _) = "~ "

    show (TokenInt n _) = (show n) ++ " "
    show (TokenBool b _) = (show b) ++ " "
    show (TokenNone _) = "None "

    show (TokenGlobal _) = "global "
    show (TokenVar (s, _)) = s ++ " "

tokenPos :: Token -> (Int, Int)
tokenPos (TokenVar (_, p)) = p
tokenPos tok = pos tok

toPos :: AlexPosn -> Pos
toPos (AlexPn _ l c) = (l,c)

}
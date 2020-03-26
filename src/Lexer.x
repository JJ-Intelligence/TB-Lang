

{ 
module Lexer where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
    $white+                             ;
    "--".*                              ;

    if                                  { \p s -> TokenIf p }
    elif                                { \p s -> TokenElif p }
    else                                { \p s -> TokenElse p }
    while                               { \p s -> TokenWhile p }
    func                                { \p s -> TokenFuncDef p }
    type                                { \p _ -> TokenFuncType p }
    \-\>                                { \p _ -> TokenReturnTypeArrow p }
    return                              { \p s -> TokenReturn p }
    for                                 { \p s -> TokenFor p }

    \;                                  { \p s -> TokenSeq p }
    \(                                  { \p s -> TokenOpenParen p }
    \)                                  { \p s -> TokenCloseParen p }
    \{                                  { \p s -> TokenOpenCurly p }
    \}                                  { \p s -> TokenCloseCurly p }
    \[                                  { \p _ -> TokenOpenSquare p }
    \]                                  { \p _ -> TokenCloseSquare p }
    \,                                  { \p _ -> TokenComma p }

    \+\+                                { \p _ -> TokenPlusPlus p }
    \-\-                                { \p _ -> TokenMinusMinus p }
    \+\=                                { \p _ -> TokenPlusAssignment p }
    \-\=                                { \p _ -> TokenMinusAssignment p }
    \*\=                                { \p _ -> TokenMultiplyAssignment p }
    \/\=                                { \p _ -> TokenDivideAssignment p }
    \^\=                                { \p _ -> TokenExponentAssignment p }
    \&\=                                { \p _ -> TokenAndAssignment p }
    \|\=                                { \p _ -> TokenOrAssignment p }

    \+                                  { \p _ -> TokenPlus p }
    \-                                  { \p _ -> TokenMinus p }
    \/                                  { \p _ -> TokenDivide p }
    \^                                  { \p _ -> TokenExponent p }
    \%                                  { \p _ -> TokenModulus p }
    \=\=                                { \p s -> TokenDoubleEquals p }
    \&\&                                { \p _ -> TokenAnd p }
    \|\|                                { \p _ -> TokenOr p }
    \<                                  { \p _ -> TokenLessThan p }
    \>                                  { \p _ -> TokenGreaterThan p }
    \=                                  { \p s -> TokenEquals p }
    \:                                  { \p _ -> TokenCons p }

    \*                                  { \p _ -> TokenStar p }
    \&                                  { \p _ -> TokenAddress p }

    Int                                 { \p _ -> TokenTypeInt p }
    Bool                                { \p _ -> TokenTypeBool p }
    NoneType                            { \p _ -> TokenTypeNone p }
    Stream                              { \p _ -> TokenTypeStream p }

    Eq                                  { \p _ -> TokenTypeConstraintEq p }
    Itr                                 { \p _ -> TokenTypeConstraintItr p }
    Ord                                 { \p _ -> TokenTypeConstraintOrd p }
    \~                                  { \p _ -> TokenTypeConstraintTwiddle p }

    $digit+                             { \p s -> TokenInt (read s) p }
    True                                { \p s -> TokenBool True p }
    False                               { \p s -> TokenBool False p }
    None                                { \p _ -> TokenNone p }

    global                              { \p _ -> TokenGlobal p }
    [$alpha \_] [$alpha $digit \_]*     { \p s -> TokenVar s p }

{

-- Lexeme Tokens.
data Token =  TokenIf               {pos :: AlexPosn}
            | TokenElif             {pos :: AlexPosn}
            | TokenElse             {pos :: AlexPosn}

            | TokenWhile            {pos :: AlexPosn}
            | TokenFor              {pos :: AlexPosn}

            | TokenReturnTypeArrow  {pos :: AlexPosn}
            | TokenFuncType         {pos :: AlexPosn}
            | TokenFuncDef          {pos :: AlexPosn}
            | TokenReturn           {pos :: AlexPosn}
            
            | TokenSeq              {pos :: AlexPosn}
            | TokenOpenParen        {pos :: AlexPosn}
            | TokenCloseParen       {pos :: AlexPosn}
            | TokenOpenCurly        {pos :: AlexPosn}
            | TokenCloseCurly       {pos :: AlexPosn}
            | TokenOpenSquare       {pos :: AlexPosn}
            | TokenCloseSquare      {pos :: AlexPosn}
            | TokenComma            {pos :: AlexPosn}

            | TokenPlusPlus         {pos :: AlexPosn}
            | TokenMinusMinus       {pos :: AlexPosn}
            | TokenPlusAssignment   {pos :: AlexPosn}
            | TokenMinusAssignment  {pos :: AlexPosn}
            | TokenMultiplyAssignment {pos :: AlexPosn}
            | TokenDivideAssignment   {pos :: AlexPosn}
            | TokenExponentAssignment {pos :: AlexPosn}
            | TokenAndAssignment    {pos :: AlexPosn}
            | TokenOrAssignment     {pos :: AlexPosn}

            | TokenPlus             {pos :: AlexPosn}
            | TokenMinus            {pos :: AlexPosn}
            | TokenDivide           {pos :: AlexPosn}
            | TokenExponent         {pos :: AlexPosn}
            | TokenModulus          {pos :: AlexPosn}
            | TokenEquals           {pos :: AlexPosn}
            | TokenDoubleEquals     {pos :: AlexPosn}
            | TokenAnd              {pos :: AlexPosn}
            | TokenOr               {pos :: AlexPosn}
            | TokenLessThan         {pos :: AlexPosn}
            | TokenGreaterThan      {pos :: AlexPosn}
            | TokenCons             {pos :: AlexPosn}

            | TokenStar             {pos :: AlexPosn}
            | TokenAddress          {pos :: AlexPosn}

            | TokenTypeInt          {pos :: AlexPosn}
            | TokenTypeBool         {pos :: AlexPosn}
            | TokenTypeNone         {pos :: AlexPosn}
            | TokenTypeStream       {pos :: AlexPosn}

            | TokenTypeConstraintEq {pos :: AlexPosn}
            | TokenTypeConstraintItr {pos :: AlexPosn}
            | TokenTypeConstraintOrd {pos :: AlexPosn}
            | TokenTypeConstraintTwiddle {pos :: AlexPosn}

            | TokenInt              {int :: Int, pos :: AlexPosn}
            | TokenBool             {bool :: Bool, pos :: AlexPosn}
            | TokenNone             {pos :: AlexPosn}

            | TokenGlobal           {pos :: AlexPosn}
            | TokenVar              {name :: String, pos :: AlexPosn} 
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
    show (TokenAnd _) = "&& "
    show (TokenOr _) = "|| "
    show (TokenLessThan _) = "< "
    show (TokenGreaterThan _) = "> "
    show (TokenCons _) = ": "

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
    show (TokenVar s _) = s ++ " "

tokenPosn :: Token -> (Int, Int)
tokenPosn tok = (l, c)
    where (AlexPn _ l c) = pos tok

}
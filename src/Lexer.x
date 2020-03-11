

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

    if                                  { \p s -> TokenIf p }
    elif                                { \p s -> TokenElif p }
    else                                { \p s -> TokenElse p }

    \;                                  { \p s -> TokenSeq p }
    \(                                  { \p s -> TokenOpenParen p }
    \)                                  { \p s -> TokenCloseParen p }
    \{                                  { \p s -> TokenOpenCurly p }
    \}                                  { \p s -> TokenCloseCurly p }
    \[                                  { \p _ -> TokenOpenSquare p }
    \]                                  { \p _ -> TokenCloseSquare p }
    \,                                  { \p _ -> TokenComma p }

    \=\=                                { \p s -> TokenDoubleEquals p }
    \=                                  { \p s -> TokenEquals p }
    \:                                  { \p _ -> TokenCons p }

    $digit+                             { \p s -> TokenInt (read s) p }
    True                                { \p s -> TokenBool True p }
    False                               { \p s -> TokenBool False p }

    [$alpha \_] [$alpha $digit \_]*     { \p s -> TokenVar s p }

 
{

-- Lexeme Tokens.
data Token =  TokenIf               {pos :: AlexPosn}
            | TokenElif             {pos :: AlexPosn}
            | TokenElse             {pos :: AlexPosn}
            
            | TokenSeq              {pos :: AlexPosn}
            | TokenOpenParen        {pos :: AlexPosn}
            | TokenCloseParen       {pos :: AlexPosn}
            | TokenOpenCurly        {pos :: AlexPosn}
            | TokenCloseCurly       {pos :: AlexPosn}
            | TokenOpenSquare       {pos :: AlexPosn}
            | TokenCloseSquare      {pos :: AlexPosn}
            | TokenComma            {pos :: AlexPosn}

            | TokenEquals           {pos :: AlexPosn}
            | TokenDoubleEquals     {pos :: AlexPosn}
            | TokenCons             {pos :: AlexPosn}

            | TokenInt              {int :: Int, pos :: AlexPosn}
            | TokenBool             {bool :: Bool, pos :: AlexPosn}

            | TokenVar              {name :: String, pos :: AlexPosn} 
            deriving (Eq) 

-- Token instance of Show, used for error messages.
instance Show Token where
    show (TokenIf _) = "if "
    show (TokenElif _) = "elif "
    show (TokenElse _) = "else "

    show (TokenSeq _) = "; "
    show (TokenOpenParen _) = "( "
    show (TokenCloseParen _) = ") "
    show (TokenOpenCurly _) = "{ "
    show (TokenCloseCurly _) = "} "
    show (TokenOpenSquare _) = "[ "
    show (TokenCloseSquare _) = "] "
    show (TokenComma _) = ", "

    show (TokenEquals _) = "= "
    show (TokenDoubleEquals _) = "== "
    show (TokenCons _) = ": "

    show (TokenInt n _) = (show n) ++ " "
    show (TokenBool b _) = (show b) ++ " "

    show (TokenVar s _) = s ++ " "

tokenPosn :: Token -> (Int, Int)
tokenPosn tok = (l, c)
    where (AlexPn _ l c) = pos tok

}
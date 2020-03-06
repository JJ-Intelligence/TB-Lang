

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

    \=\=                                 { \p s -> TokenDoubleEquals p }
    \=                                  { \p s -> TokenEquals p }

    $digit+                             { \p s -> TokenInt (read s) p }
    True                                { \p s -> TokenBool True p }
    False                               { \p s -> TokenBool False p }

    [$alpha \_] [$alpha $digit \_]*     { \p s -> TokenVar s p }

 
{ 
data Token =  TokenIf               {pos :: AlexPosn}
            | TokenElif             {pos :: AlexPosn}
            | TokenElse             {pos :: AlexPosn}
            
            | TokenSeq              {pos :: AlexPosn}
            | TokenOpenParen        {pos :: AlexPosn}
            | TokenCloseParen       {pos :: AlexPosn}
            | TokenOpenCurly        {pos :: AlexPosn}
            | TokenCloseCurly       {pos :: AlexPosn}

            | TokenEquals           {pos :: AlexPosn}
            | TokenDoubleEquals     {pos :: AlexPosn}

            | TokenInt              {int :: Int, pos :: AlexPosn}
            | TokenBool             {bool :: Bool, pos :: AlexPosn}

            | TokenVar              {name :: String, pos :: AlexPosn} 
    deriving (Eq,Show) 

tokenPosn :: Token -> (Int, Int)
tokenPosn tok = (l, c)
    where (AlexPn _ l c) = pos tok

}


{ 
module ToyTokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
    $white+                             ;



    $alpha [$alpha $digit \_ \’]*       { \s -> TokenVar s }

 
{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = TokenVar {name :: String, pos :: AlexPosn} 
    deriving (Eq,Show) 

tokenPosn :: Token -> (Int, Int)
tokenPosn tok = (l, c)
    where (AlexPn _ l c) = pos tok

}
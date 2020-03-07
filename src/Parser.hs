{-# OPTIONS_GHC -w #-}
module Parser where 
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,82) ([16640,16412,1808,4224,0,0,0,0,0,1,28932,0,0,0,8192,8192,4,7233,4160,4103,452,1568,16640,28,74,0,0,4,0,16896,0,4,24,0,0,4,1024,0,0,7233,18944,0,4,24,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","E","EElif","B","O","L","if","elif","else","';'","'('","')'","'{'","'}'","'=='","'='","int","bool","var","%eof"]
        bit_start = st * 22
        bit_end = (st + 1) * 22
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..21]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (9) = happyShift action_6
action_0 (15) = happyShift action_7
action_0 (19) = happyShift action_8
action_0 (20) = happyShift action_9
action_0 (21) = happyShift action_10
action_0 (4) = happyGoto action_11
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (9) = happyShift action_6
action_1 (15) = happyShift action_7
action_1 (19) = happyShift action_8
action_1 (20) = happyShift action_9
action_1 (21) = happyShift action_10
action_1 (4) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (12) = happyShift action_12
action_2 (17) = happyShift action_13
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_7

action_4 _ = happyReduce_9

action_5 _ = happyReduce_8

action_6 (13) = happyShift action_16
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (9) = happyShift action_6
action_7 (15) = happyShift action_7
action_7 (19) = happyShift action_8
action_7 (20) = happyShift action_9
action_7 (21) = happyShift action_10
action_7 (4) = happyGoto action_15
action_7 (6) = happyGoto action_3
action_7 (7) = happyGoto action_4
action_7 (8) = happyGoto action_5
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_15

action_9 _ = happyReduce_16

action_10 (18) = happyShift action_14
action_10 _ = happyReduce_6

action_11 (12) = happyShift action_12
action_11 (17) = happyShift action_13
action_11 (22) = happyAccept
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (9) = happyShift action_6
action_12 (15) = happyShift action_7
action_12 (19) = happyShift action_8
action_12 (20) = happyShift action_9
action_12 (21) = happyShift action_10
action_12 (4) = happyGoto action_21
action_12 (6) = happyGoto action_3
action_12 (7) = happyGoto action_4
action_12 (8) = happyGoto action_5
action_12 _ = happyReduce_2

action_13 (9) = happyShift action_6
action_13 (15) = happyShift action_7
action_13 (19) = happyShift action_8
action_13 (20) = happyShift action_9
action_13 (21) = happyShift action_10
action_13 (4) = happyGoto action_20
action_13 (6) = happyGoto action_3
action_13 (7) = happyGoto action_4
action_13 (8) = happyGoto action_5
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (9) = happyShift action_6
action_14 (15) = happyShift action_7
action_14 (19) = happyShift action_8
action_14 (20) = happyShift action_9
action_14 (21) = happyShift action_10
action_14 (4) = happyGoto action_19
action_14 (6) = happyGoto action_3
action_14 (7) = happyGoto action_4
action_14 (8) = happyGoto action_5
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (12) = happyShift action_12
action_15 (16) = happyShift action_18
action_15 (17) = happyShift action_13
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (9) = happyShift action_6
action_16 (15) = happyShift action_7
action_16 (19) = happyShift action_8
action_16 (20) = happyShift action_9
action_16 (21) = happyShift action_10
action_16 (4) = happyGoto action_17
action_16 (6) = happyGoto action_3
action_16 (7) = happyGoto action_4
action_16 (8) = happyGoto action_5
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (12) = happyShift action_12
action_17 (14) = happyShift action_22
action_17 (17) = happyShift action_13
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_13

action_19 (17) = happyShift action_13
action_19 _ = happyReduce_5

action_20 _ = happyReduce_14

action_21 (12) = happyShift action_12
action_21 (17) = happyShift action_13
action_21 _ = happyReduce_1

action_22 (15) = happyShift action_7
action_22 (6) = happyGoto action_23
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (10) = happyShift action_25
action_23 (11) = happyShift action_26
action_23 (5) = happyGoto action_24
action_23 _ = happyReduce_4

action_24 _ = happyReduce_3

action_25 (13) = happyShift action_28
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (15) = happyShift action_7
action_26 (6) = happyGoto action_27
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_12

action_28 (9) = happyShift action_6
action_28 (15) = happyShift action_7
action_28 (19) = happyShift action_8
action_28 (20) = happyShift action_9
action_28 (21) = happyShift action_10
action_28 (4) = happyGoto action_29
action_28 (6) = happyGoto action_3
action_28 (7) = happyGoto action_4
action_28 (8) = happyGoto action_5
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (12) = happyShift action_12
action_29 (14) = happyShift action_30
action_29 (17) = happyShift action_13
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (15) = happyShift action_7
action_30 (6) = happyGoto action_31
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (10) = happyShift action_25
action_31 (11) = happyShift action_26
action_31 (5) = happyGoto action_32
action_31 _ = happyReduce_11

action_32 _ = happyReduce_10

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 6 4 happyReduction_3
happyReduction_3 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_3 happy_var_5 (Just happy_var_6)
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 5 4 happyReduction_4
happyReduction_4 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_3 happy_var_5 Nothing
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1 _))
	 =  HappyAbsSyn4
		 (DefVar happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  4 happyReduction_6
happyReduction_6 (HappyTerminal (TokenVar happy_var_1 _))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  4 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  4 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  4 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happyReduce 6 5 happyReduction_10
happyReduction_10 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Elif happy_var_3 happy_var_5 (Just happy_var_6)
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 5 5 happyReduction_11
happyReduction_11 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Elif happy_var_3 happy_var_5 Nothing
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_2  5 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Else happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  6 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (Op (CompOp Equality happy_var_1 happy_var_3)
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 (HappyTerminal (TokenInt happy_var_1 _))
	 =  HappyAbsSyn8
		 (Literal (EInt happy_var_1)
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  8 happyReduction_16
happyReduction_16 (HappyTerminal (TokenBool happy_var_1 _))
	 =  HappyAbsSyn8
		 (Literal (EBool happy_var_1)
	)
happyReduction_16 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 22 22 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenIf _ -> cont 9;
	TokenElif _ -> cont 10;
	TokenElse _ -> cont 11;
	TokenSeq _ -> cont 12;
	TokenOpenParen _ -> cont 13;
	TokenCloseParen _ -> cont 14;
	TokenOpenCurly _ -> cont 15;
	TokenCloseCurly _ -> cont 16;
	TokenDoubleEquals _ -> cont 17;
	TokenEquals _ -> cont 18;
	TokenInt happy_dollar_dollar _ -> cont 19;
	TokenBool happy_dollar_dollar _ -> cont 20;
	TokenVar happy_dollar_dollar _ -> cont 21;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 22 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "D:/GitHub/haskell-platform/build/ghc-bindist/local/lib/include/ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "F:/Users/randy/AppData/Local/Temp/ghc1900_0/ghc_2.h" #-}


























































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates\\\\GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 75 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 84 "templates\\\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 147 "templates\\\\GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates\\\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates\\\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

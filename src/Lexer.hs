{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LINE 3 ".\Lexer.x" #-}
 
module Lexer where 

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Array.Base (unsafeAt)
#else
import Array
#endif
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.






import Data.Word (Word8)
















import Data.Char (ord)
import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]



type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))





























































-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad
















































































































-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)







































































































-- -----------------------------------------------------------------------------
-- Basic wrapper

























-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version
































-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.


--alexScanTokens :: String -> [token]
alexScanTokens str0 = go (alexStartPos,'\n',[],str0)
  where go inp__@(pos,_,_,str) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act pos (take len str) : go inp__'



-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version














-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.














alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Array Int Int
alex_base = listArray (0 :: Int, 30)
  [ -8
  , -33
  , -54
  , -53
  , 2
  , 70
  , 145
  , 220
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 295
  , 305
  , 380
  , 455
  , 530
  , 605
  , 680
  , 755
  , 830
  , 905
  , 980
  , 1055
  , 1130
  , 1205
  , 1280
  , 1355
  ]

alex_table :: Array Int Int
alex_table = listArray (0 :: Int, 1610)
  [ 0
  , 4
  , 4
  , 4
  , 4
  , 4
  , 13
  , 1
  , 2
  , 0
  , 0
  , 4
  , 4
  , 4
  , 4
  , 4
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 4
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 3
  , 9
  , 10
  , 4
  , 0
  , 0
  , 0
  , 0
  , 0
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 0
  , 8
  , 0
  , 14
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 24
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 27
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 28
  , 25
  , 25
  , 25
  , 30
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 11
  , 0
  , 12
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 7
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 16
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 17
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 29
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 18
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 19
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 20
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 26
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 23
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 22
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 21
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 6
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 25
  , 0
  , 25
  , 25
  , 25
  , 25
  , 25
  , 5
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 25
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  ]

alex_check :: Array Int Int
alex_check = listArray (0 :: Int, 1610)
  [ -1
  , 9
  , 10
  , 11
  , 12
  , 13
  , 39
  , 61
  , 61
  , -1
  , -1
  , 9
  , 10
  , 11
  , 12
  , 13
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 32
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 39
  , 40
  , 41
  , 32
  , -1
  , -1
  , -1
  , -1
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , 59
  , -1
  , 61
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 123
  , -1
  , 125
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_deflt :: Array Int Int
alex_deflt = listArray (0 :: Int, 30)
  [ -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_accept = listArray (0 :: Int, 30)
  [ AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccSkip
  , AlexAcc 25
  , AlexAcc 24
  , AlexAcc 23
  , AlexAcc 22
  , AlexAcc 21
  , AlexAcc 20
  , AlexAcc 19
  , AlexAcc 18
  , AlexAcc 17
  , AlexAcc 16
  , AlexAcc 15
  , AlexAcc 14
  , AlexAcc 13
  , AlexAcc 12
  , AlexAcc 11
  , AlexAcc 10
  , AlexAcc 9
  , AlexAcc 8
  , AlexAcc 7
  , AlexAcc 6
  , AlexAcc 5
  , AlexAcc 4
  , AlexAcc 3
  , AlexAcc 2
  , AlexAcc 1
  , AlexAcc 0
  ]

alex_actions = array (0 :: Int, 26)
  [ (25,alex_action_1)
  , (24,alex_action_2)
  , (23,alex_action_3)
  , (22,alex_action_4)
  , (21,alex_action_5)
  , (20,alex_action_6)
  , (19,alex_action_7)
  , (18,alex_action_8)
  , (17,alex_action_9)
  , (16,alex_action_10)
  , (15,alex_action_11)
  , (14,alex_action_12)
  , (13,alex_action_13)
  , (12,alex_action_14)
  , (11,alex_action_14)
  , (10,alex_action_14)
  , (9,alex_action_14)
  , (8,alex_action_14)
  , (7,alex_action_14)
  , (6,alex_action_14)
  , (5,alex_action_14)
  , (4,alex_action_14)
  , (3,alex_action_14)
  , (2,alex_action_14)
  , (1,alex_action_14)
  , (0,alex_action_14)
  ]

{-# LINE 36 ".\Lexer.x" #-}
 
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


alex_action_1 =  \p s -> TokenIf p 
alex_action_2 =  \p s -> TokenElif p 
alex_action_3 =  \p s -> TokenElse p 
alex_action_4 =  \p s -> TokenSeq p 
alex_action_5 =  \p s -> TokenOpenParen p 
alex_action_6 =  \p s -> TokenCloseParen p 
alex_action_7 =  \p s -> TokenOpenCurly p 
alex_action_8 =  \p s -> TokenCloseCurly p 
alex_action_9 =  \p s -> TokenDoubleEquals p 
alex_action_10 =  \p s -> TokenEquals p 
alex_action_11 =  \p s -> TokenInt (read s) p 
alex_action_12 =  \p s -> TokenBool True p 
alex_action_13 =  \p s -> TokenBool False p 
alex_action_14 =  \p s -> TokenVar s p 
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine































































alexIndexInt16OffAddr arr off = arr ! off




















alexIndexInt32OffAddr arr off = arr ! off











quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input__ (sc)
  = alexScanUser undefined input__ (sc)

alexScanUser user__ input__ (sc)
  = case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
  (AlexNone, input__') ->
    case alexGetByte input__ of
      Nothing ->



                                   AlexEOF
      Just _ ->



                                   AlexError input__'

  (AlexLastSkip input__'' len, _) ->



    AlexSkip input__'' len

  (AlexLastAcc k input__''' len, _) ->



    AlexToken input__''' len (alex_actions ! k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user__ orig_input len input__ s last_acc =
  input__ `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input__ of
     Nothing -> (new_acc, input__)
     Just (c, new_input) ->



      case fromIntegral c of { (ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = (base + ord_c)
                check  = alexIndexInt16OffAddr alex_check offset

                new_s = if (offset >= (0)) && (check == ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            (-1) -> (new_acc, input__)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user__ orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
                        new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ (len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ (len)

        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastAcc a input__ (len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastSkip input__ (len)
           | otherwise
           = check_accs rest


data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip

  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user__ in1 len in2
  = p1 user__ in1 len in2 && p2 user__ in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input__ _ _ = c == alexInputPrevChar input__

alexPrevCharMatches f _ input__ _ _ = f (alexInputPrevChar input__)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input__ _ _ = arr ! alexInputPrevChar input__

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user__ _ _ input__ =
     case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.


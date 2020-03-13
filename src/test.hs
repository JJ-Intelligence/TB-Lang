
import Evaluator
import Parser

getValue :: State -> Expr
getValue (x, _, _, _) = x

--  (Expr, Environment, Store, Kon)
simpleTests :: [[Bool]]
simpleTests =
    [
        [ -- Basic value eval
            (getValue $ interpret "1") == Value (VInt 1),
            (getValue $ interpret "-2") == Value (VInt (-2)),
            (getValue $ interpret "True") == Value (VBool True),
            (getValue $ interpret "False") == Value (VBool False)
        ],
        [ -- Basic lists stuff eval
            (getValue $ interpret "[]") == Value (VList []),
            (getValue $ interpret "[1]") == Value (VList [VInt 1]),
            (getValue $ interpret "[True]") == Value (VList [VBool True]),
            (getValue $ interpret "[1, 2]") == Value (VList [VInt 1, VInt 2]),
            (getValue $ interpret "[[1], [3]]") == Value (VList [VList [VInt 1], VList [VInt 3]]),
            (getValue $ interpret "[[True], [False]]") == Value (VList [VList [VBool True], VList [VBool False]])
        ],
        [ -- Operators
            (getValue $ interpret "1+2") == Value (VInt 3),
            (getValue $ interpret "1-2") == Value (VInt (-1)),
            (getValue $ interpret "4/2") == Value (VInt 2),
            (getValue $ interpret "3/2") == Value (VInt 1),
            (getValue $ interpret "3*2") == Value (VInt 6),
            (getValue $ interpret "1<2") == Value (VBool True),
            (getValue $ interpret "2>1") == Value (VBool True),
            (getValue $ interpret "1>2") == Value (VBool False),
            (getValue $ interpret "2<1") == Value (VBool False),
            (getValue $ interpret "6%2") == Value (VInt 0),
            (getValue $ interpret "True && True") == Value (VBool True),
            (getValue $ interpret "False && True") == Value (VBool False),
            (getValue $ interpret "False || True") == Value (VBool True),
            (getValue $ interpret "True || False") == Value (VBool True),
            (getValue $ interpret "False || False") == Value (VBool False),
            (getValue $ interpret "3 == 3") == Value (VBool True),
            (getValue $ interpret "5 == 1") == Value (VBool False)
        ],
        [ -- Compound operators
            (getValue $ interpret "1+2+6") == Value (VInt 9),
            (getValue $ interpret "1+2*6") == Value (VInt 13),
            (getValue $ interpret "(1+2)*6") == Value (VInt 18),
            (getValue $ interpret "1+(2*6)") == Value (VInt 13),
            (getValue $ interpret "2/(2+6)") == Value (VInt 0),
            (getValue $ interpret "1+(2*6)*2") == Value (VInt 25)
        ],
        [ -- Conditional expressions
            (getValue $ interpret "if (True) {3} else {1}") == Value (VInt 3),
            (getValue $ interpret "if (False) {3} else {1}") == Value (VInt 1),
            (getValue $ interpret "if (1 < 2) {3} else {1}") == Value (VInt 3),
            (getValue $ interpret "if (1 > 2) {3} else {1}") == Value (VInt 1),
            (getValue $ interpret "if (1 > 0) {1} elif (3 < 4) {2} else {3}") == Value (VInt 1),
            (getValue $ interpret "if (1 > 2) {1} elif (3 < 4) {2} else {3}") == Value (VInt 2),
            (getValue $ interpret "if (1 > 2) {1} elif (3 < 2) {2} else {3}") == Value (VInt 3),
            (getValue $ interpret "if (False) {1} elif (False) {2} elif (True) {3} else {4}") == Value (VInt 3),
        ]
    ]



-- The main program checks and displays the results of the tests
--
main :: IO ()
main =
  do
    putStrLn "... Testing ..."
    simpleTestSuite simpleTests
    putStrLn "... Completed ..."

fails :: [Bool] -> Int -> String
fails [] _ = ""
fails (x:xs) i | x==True = fails (xs) (i+1)
               | otherwise = ("\n\t\tTest failed: " ++ (show (i))) ++ fails (xs) (i+1)

-- process one test suite at a time
simpleTestSuite :: [[Bool]] -> IO ()
simpleTestSuite [] =
  do
    putStr ""
simpleTestSuite (bs : bbs) =
  do
    putStrLn ("  " ++ show (length [b | b <- bs, b]) ++ " tests passed out of " ++ show (length bs) ++ (fails [b | b <- bs] 0))
    simpleTestSuite bbs
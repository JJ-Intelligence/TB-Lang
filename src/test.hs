
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
            (getValue $ interpret "[]") == Value (VList [])
--             (getValue $ interpret "-2") == Value (VInt (-2)),
--             (getValue $ interpret "True") == Value (VBool True),
--             (getValue $ interpret "False") == Value (VBool False)
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

-- process one test suite at a time
simpleTestSuite :: [[Bool]] -> IO ()
simpleTestSuite [] =
  do
    putStr ""
simpleTestSuite (bs : bbs) =
  do
    putStrLn ("  " ++ show (length [b | b <- bs, b]) ++ " tests passed out of " ++ show (length bs))
    simpleTestSuite bbs
module Main where
import System.Environment
import Evaluator
import Lexer
import Parser

main :: IO ()
main = do args <- getArgs
          case args of
                [file] -> do f <- readFile file
                             let xs = parse $ alexScanTokens f
                             print "Parsed: "
                             print xs
                             putStr "\n"
                             print "Evaluating: "
                             startEvaluator xs
                             -- print "Interpreted: "
                             -- print $ interpret f
                _ -> print "Wrong number of arguments"
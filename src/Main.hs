module Main where
import System.Environment
import Evaluator
import Lexer
import Parser

main :: IO ()
main = do args <- getArgs
          case args of
            [solution] -> do 
                    f <- readFile solution
                    let xs = parse $ alexScanTokens f
                    print "Parsed: "
                    print xs
                    putStr "\n"
                    print "Evaluating: "
                    startEvaluator xs
            _ -> print "Wrong number of arguments"
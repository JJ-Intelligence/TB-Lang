module Main where
import System.Environment
import Evaluator
import Lexer
import Parser
import Preprocessor

main :: IO ()
main = do args <- getArgs
          case args of
            [solution] -> do 
                    f <- readFile solution
                    let xs = parse $ alexScanTokens f
                    print "Parsed: "
                    print xs
                    putStr "\n"
                    preprocess xs
                    print "Evaluating: "
                    startEvaluator xs
            _ -> print "Wrong number of arguments"
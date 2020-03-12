module Main where
import System.Environment
import Lexer
import Parser
import Evaluator

main :: IO ()
main = do args <- getArgs
          case args of
                [file] -> do f <- readFile file
                             print $ eval $ parse $ alexScanTokens f
                _ -> print "Wrong number of arguments"
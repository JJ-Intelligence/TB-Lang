module Main where
import System.Environment
import Lexer
import Parser
import Evaluator

main :: IO ()
main = do args <- getArgs
          case args of
                [file] -> do f <- readFile file
                             let xs = alexScanTokens f
                             print $ parse xs
                             print ""
                             print $ eval $ parse xs
                _ -> print "Wrong number of arguments"
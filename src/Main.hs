module Main where
import System.Environment
import Evaluator
import Lexer
import Parser


main :: IO ()
main = do args <- getArgs
          case args of
                [file] -> do f <- readFile file
                             let xs = alexScanTokens f
                             print $ parse xs
                             print "\n"
                             print $ eval $ parse xs
                             print "\n"
                             print $ interpret f
                _ -> print "Wrong number of arguments"
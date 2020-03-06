module Main where
import System.Environment
import Lexer
import Parser

main :: IO ()
main = do args <- getArgs
          case args of
                [file] -> do f <- readFile file
                             let xs = alexScanTokens f
                             print xs
                             print $ parse xs
                _ -> print "Wrong number of arguments"
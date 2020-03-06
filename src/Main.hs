module Main where
import System.Environment
import Lexer

main :: IO ()
main = do args <- getArgs
          case args of
                [file] -> do f <- readFile file
                             print $ alexScanTokens f
                _ -> print "Wrong number of arguments"
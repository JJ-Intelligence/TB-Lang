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
                             let (val, _, _, _) = eval xs
                             print "Evaluated: "
                             print $ eval xs
                             putStr "\n"
                             print "Evaluated value: "
                             print val
                             putStr "\n"
                             print "Interpreted: "
                             print $ interpret f
                _ -> print "Wrong number of arguments"
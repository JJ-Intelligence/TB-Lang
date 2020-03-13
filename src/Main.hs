module Main where
import System.Environment
import Evaluator


main :: IO ()
main = do args <- getArgs
          case args of
                [file] -> do f <- readFile file
                             print $ interpret f
                _ -> print "Wrong number of arguments"
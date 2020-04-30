module Main where

import System.Environment (getArgs)
import System.FilePath.Posix (takeExtension)
import System.Exit (exitFailure)

import Expression (printStdErr)
import Evaluator
import Lexer
import Parser
import Preprocessor

-- Run with one .spl file (otherwise an error is thrown to stderr).
-- The interpreter will read in the file contents; tokenize it using the lexer; parse the tokens; 
-- put the resulting AST through the preprocessor; evaluate the AST (if there have been no lexical, parsing, or type errors).
main :: IO ()
main = do args <- getArgs
          case args of
            [solution] -> do 
                case ".spl" == (takeExtension solution) of
                    False -> do
                        printStdErr ("Input ERROR: inputted file is not a .spl file, so could not be interpreted")
                        exitFailure
                    True -> do
                        f <- readFile solution
                        let xs = parse $ alexScanTokens f
--                        print "Parsed: " -- TODO - remove me{-
--                        print xs -- TODO - remove me
--                        putStr "\n" -- TODO - remove me
--                        print "Preprocessing: " -- TODO - remove me
                        preprocess xs
--                        print "Evaluating: " -- TODO - remove me-}
                        startEvaluator xs
            _ -> do
                printStdErr ("Input ERROR: multiple files inputted, the interpreted can only read one file")
                exitFailure
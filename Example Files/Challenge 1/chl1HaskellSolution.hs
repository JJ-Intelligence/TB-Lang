module Chl1HaskellSolution where

import qualified Data.Time.Clock.POSIX as Time -- DEBUGGING
getTime = round `fmap` Time.getPOSIXTime -- DEBUGGING

main :: IO ()
main = do
    content <- readFile "largeInput.txt"
    let line = foldr (\x acc -> map (read :: String -> Int) (words x) : acc) [] $ lines content
    solve (map (!!0) line) (map (!!1) line)

printInt a = do
    t <- getTime
    putStrLn $ (show a) ++ " : " ++ (show t)

solve :: [Int] -> [Int] -> [Int]
solve [] bs = return ()
solve [a] bs = printInt a
solve (a:a':as) (b:bs) = do
    printInt a
    printInt a'
    printInt b
    solve as bs

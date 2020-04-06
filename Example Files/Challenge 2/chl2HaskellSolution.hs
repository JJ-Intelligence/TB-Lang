module Chl2HaskellSolution where

main :: IO ()
main = do
    content <- readFile "input.txt"
    let line = foldr (\x acc -> map (read :: String -> Int) (words x) : acc) [] $ lines content
    writeFile "exp.txt" $ foldr (\i acc -> (show i) ++ "\n" ++ acc) "" (solve (map (!!0) line) (map (!!1) line) (map (!!2) line))

solve :: [Int] -> [Int] -> [Int] -> [Int]
solve [] [] [] = []
solve (a:as) (b:bs) (c:cs) = c : b : a : (a+b) : (b+c) : solve as bs cs
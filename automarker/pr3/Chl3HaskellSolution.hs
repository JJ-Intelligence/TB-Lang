module Chl2HaskellSolution where

main :: IO ()
main = do
    content <- readFile "input.txt"
    let line = foldr (\x acc -> map (read :: String -> Int) (words x) : acc) [] $ lines content
    writeFile "exp.txt" $ foldr (\i acc -> (show i) ++ "\n" ++ acc) "" (solve (map (!!0) line) (map (!!1) line))

solve :: [Int] -> [Int] -> [Int]
solve _ [] = []
solve (a:as) (_:b:bs) = a : b : solve as bs
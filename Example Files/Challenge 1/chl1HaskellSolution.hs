module chl1HaskellSolution where

main :: IO ()
main = do
    line <- (read :: String -> Int) $ words getLine
    writeFile "exp1.txt" $ foldr (\i acc -> i ++ "\n" ++ acc) "" (solve (line!!0) (line!!1))

solve :: [Int] -> [Int] -> [Int]
solve [] bs = []
solve [a] bs = [a]
solve (a:a':as) (b:bs) = a : a' : b : solve as bs


module chl4HaskellSolution where

main :: IO ()
main = do
    line <- (read :: String -> Int) $ words getLine
    writeFile "exp4.txt" $ foldr (\i acc -> i ++ "\n" ++ acc) "" (solve (line!!0))

solve :: [Int] -> [Int]
solve [] = []
solve (a1:a2:a3:as) = a3 : (2*a2) : (3*a1-1) : solve as
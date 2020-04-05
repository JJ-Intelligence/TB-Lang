module chl2HaskellSolution where

main :: IO ()
main = do
    line <- (read :: String -> Int) $ words getLine
    writeFile "exp2.txt" $ foldr (\i acc -> i ++ "\n" ++ acc) "" (solve (line!!0) (line!!1) (line!!2))

solve :: [Int] -> [Int] -> [Int] -> [Int]
solve [] [] [] = []
solve (a:as) (b:bs) (c:cs) = c : b : a : (a+b) : (b+c) : solve as bs cs
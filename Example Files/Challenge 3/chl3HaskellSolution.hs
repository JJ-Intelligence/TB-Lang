module chl2HaskellSolution where

main :: IO ()
main = do
    line <- (read :: String -> Int) $ words getLine
    writeFile "exp3.txt" $ 0 : (foldr (\i acc -> i ++ "\n" ++ acc) "" (solve (line!!0) (line!!1)))

solve :: [Int] -> [Int] -> [Int]
solve _ [] = []
solve (a:as) (_:b:bs) = a : b : solve as bs
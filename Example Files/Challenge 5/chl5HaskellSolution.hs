module chl5HaskellSolution where

main :: IO ()
main = do
    line <- (read :: String -> Int) $ words getLine
    writeFile "exp5.txt" $ foldr (\i acc -> i ++ "\n" ++ acc) "" (solve (line!!0))

solve :: [Int] -> [Int]
solve [] = []
solve (a:as) = a : map (+a) $ solve as
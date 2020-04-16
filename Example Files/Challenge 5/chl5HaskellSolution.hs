module chl5HaskellSolution where

main :: IO ()
main = do
    content <- readFile "input.txt"
    let line = foldr (\x acc -> map (read :: String -> Int) (words x) : acc) [] $ lines content
    writeFile "exp.txt" $ foldr (\i acc -> (show i) ++ "\n" ++ acc) "" (solve (map (!!0) line))

solve :: [Int] -> [Int]
solve [] = []
solve (a:as) = a : map (+a) $ solve as
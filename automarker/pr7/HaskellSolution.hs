module HaskellSolution where

main :: IO ()
main = do
    content <- readFile "input.txt"
    let line = foldr (\x acc -> map (read :: String -> Int) (words x) : acc) [] $ lines content
    writeFile "exp.txt" $ foldr (\i acc -> (show i) ++ "\n" ++ acc) "" (solve (map (!!0) line))

solve :: [Int] -> [Int]
solve as | length as < 4 = []
solve (a:a':a'':as) = a'' : (solve' as)
solve _ = []

solve' as | length as < 4 = []
solve' (a:a':a'':a''':as) = a''' : (solve as)
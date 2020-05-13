module HaskellSolution where

main :: IO ()
main = do
    content <- readFile "input.txt"
    let line = foldr (\x acc -> map (read :: String -> Int) (words x) : acc) [] $ lines content
    writeFile "exp.txt" $ foldr (\i acc -> (show i) ++ "\n" ++ acc) "" (solve (map (!!0) line) (map (!!1) line))

solve :: [Int] -> [Int] -> [Int]
solve as bs
    | length as < 5 || length bs < 5 = out as bs
    | otherwise = let diff (take 5 as) (take 5 bs)
    | otherwise = (out (take 5 as) (take 5 bs)) ++ [(diff (take 5 as) (take 5 bs))] ++ (solve (drop 5 as) (drop 5 bs))

out [] [] = []
out _ [] = []
out [] _ = []
out (a:as) (b:bs) = [a, b] ++ (out as bs)

diff [] [] = 0
diff (a:as) (b:bs) = (a - b) + (diff as bs)
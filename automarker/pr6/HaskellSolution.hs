module HaskellSolution where

main :: IO ()
main = do
    content <- readFile "input.txt"
    let line = foldr (\x acc -> map (read :: String -> Int) (words x) : acc) [] $ lines content
    writeFile "exp.txt" $ foldr (\i acc -> (show i) ++ "\n" ++ acc) "" (solve (map (!!0) line) (map (!!1) line))

solve :: [Int] -> [Int] -> [Int]
solve [] [] = []
solve (a:[]) [] = [a]
solve (a:a':as) [] = [a,a']
solve (a:a':as) (b:[]) = [a,a',b]
solve (a:a':as) (b:b':[]) = [a,a',b,b']
solve (a:a':as) (b:b':b'':bs) = [a,a',b,b',b''] ++ (solve as bs)

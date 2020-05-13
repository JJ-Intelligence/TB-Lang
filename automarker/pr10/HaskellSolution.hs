module HaskellSolution where

main :: IO ()
main = do
    content <- readFile "input.txt"
    let line = foldr (\x acc -> map (read :: String -> Int) (words x) : acc) [] $ lines content
    writeFile "exp.txt" $ foldr (\i acc -> (show i) ++ "\n" ++ acc) "" (solve (map (!!0) line) [1,1] 1)

solve :: [Int] -> [Int] -> Int -> [Int]
solve as fib n
    | length as >= n = (sum [x*y | (x,y) <- zip (take n as) (drop 1 fib)]) : (solve as (newFib fib) (n+1))
    | otherwise = []

newFib fib = (fib!!0 + fib!!1) : fib


-- fib = [1, 1]; -- List to store fib elements in

-- -- Keep iterating until out of input
-- for (i = 1; hasElems(i, in(0)); i++) {

--     -- Look at the first i elements, but don't remove from stream
--     xs = peekN(i, in(0));

--     -- Sum up terms in sequence: fib * a_n
--     sum = 0;
--     for (j = 0; j < length(xs); j++) {
--         sum += get(j+1, fib) * get(j, xs);
--     };
--     out(sum);

--     -- Add next term to fib sequence
--     fib = (get(0, fib) + get(1, fib)) : fib;
-- };

module GenerateInput where

main :: IO ()
main = do
    let start = -1000
    let colLen = 100007
    let numStreams = 1
    writeFile "input.txt" $ genString start numStreams colLen 

genString :: Int -> Int -> Int -> String
genString _ _ 0 = []
genString i n l = xs ++ genString i' n (l-1) 
    where
        (i', xs) = helper i n ""

        helper i 0  ls = (i, ls ++ "\n")
        helper i n ls = helper (i+1) (n-1) ((show i) ++ " " ++ ls)
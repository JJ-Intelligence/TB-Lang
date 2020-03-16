
import qualified Data.Map.Strict as MapS
import qualified Data.IntMap.Lazy as MapL

solve m k
    | k == 0 = m
    | otherwise = solve (MapS.update (\x -> Just (2:x)) 0 m) (k-1)

newSolve m k
    | k == 0 = m
    | otherwise = newSolve (MapL.update (\x -> Just (1:x)) 0 m) (k-1) 
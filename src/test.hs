module Test where
import qualified Data.IntMap.Lazy as M
import Data.Maybe

solve 0 map = return map
solve n map = do
    let map' = M.update (\ys -> Just (1:1:ys)) 1 $ M.update (\xs -> Just (0:0:xs)) 0 map
    let r = (take 2 $ fromJust $ M.lookup 0 map'):(take 1 $ fromJust $ M.lookup 1 map'):[]
    -- putStrLn $ show r
    solve (n-1) (M.update (\ys -> Just (drop 1 ys)) 1 $ M.update (\xs -> Just (drop 2 xs)) 0 map')
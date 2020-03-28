module Preprocessor where
import Expression
import qualified Data.Map.Strict as Map
import Data.Maybe
-- import Data.List
import Debug.Trace
import System.IO (hPutStrLn, stderr)

type TStore = Map.Map String [Type]

type ProcessState = (TStore, TStore)

preprocess :: Expr -> IO ()
preprocess e = do
    (t, _) <- process e (Map.empty, Map.empty)
    return ()

process :: Expr -> ProcessState -> IO (Type, ProcessState)
process (Seq e1 e2) (global, local) = do
    (t', (global', local')) <- process e1 (global, local)
    process e2 (global', local')

process (Literal (EInt n)) (global, local) = return (TInt, (global, local))
process (Literal (EBool b)) (global, local) = return (TBool, (global, local))
process (Literal Empty) (global, local) = return (TList TEmpty, (global, local))
process (Literal ENone) (global, local) = return (TNone, (global, local))

--process (LocalAssign (DefVar s (FuncType ps out cs)), )
--process (LocalAssign (DefVar s (Func ps e1)), )

process (LocalAssign (DefVar s e1)) (global, local) = do
    (t', (global', local')) <- process e1 (global, local)
    let lv = (lookupT s (global, local))
    if lv /= Nothing && (fromJust lv) !! 0 /= t'
        then  hPutStrLn stderr ("WARNING: type changed: "++(show s))
        else return ()
    return (t', (global', Map.insert s [t'] local'))

process e _ = error (show e)

lookupT :: String -> ProcessState -> Maybe [Type]
lookupT name (global, local)
    | lLookup /= Nothing = lLookup
    | gLookup /= Nothing = gLookup
    | otherwise = Nothing
    where
        lLookup = Map.lookup name local
        gLookup = Map.lookup name global


--process (Seq e1 e2, t, global, local) =



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
    if lv /= Nothing && (fromJust lv) !! 0 /= t' -- Check type hasn't changed
        then printStdErr ("WARNING: type changed: "++(show s))
        else return ()
    return (t', (global', Map.insert s [t'] local'))

process (Var s) state = do
    let t = (lookupT s state)
    if t == Nothing
        then printStdErr ("WARNING: variable referenced before assignment: "++(show s))
        else return ()
    return ((fromJust t)!!0, state)

-- Make dodgy assumption that MathOps all return ints - don't judge me josh
process (Op (MathOp op e1 e2)) (global, local)= do
    (t1, (global1, local1)) <- process e1 (global, local)
    (t2, (global2, local2)) <- process e2 (global1, local1)
    if t1 /= TInt
        then printStdErr ("WARNING: type invalid for op: "++(show e1))
        else return ()
    if t2 /= TInt
        then printStdErr ("WARNING: type invalid for op: "++(show e2))
        else return ()
    return (TInt, (global2, local2))

process e _ = error (show e)

lookupT :: String -> ProcessState -> Maybe [Type]
lookupT name (global, local)
    | lLookup /= Nothing = lLookup
    | gLookup /= Nothing = gLookup
    | otherwise = Nothing
    where
        lLookup = Map.lookup name local
        gLookup = Map.lookup name global

printStdErr s = do
    hPutStrLn stderr s
    return ()


--process (Seq e1 e2, t, global, local) =



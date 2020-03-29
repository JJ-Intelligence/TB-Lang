module Preprocessor where
import Expression
import qualified Data.Map.Strict as Map
import Data.Maybe
-- import Data.List
import Debug.Trace
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

type TStore = Map.Map String [Type]

type ProcessState = (TStore, TStore)

preprocess :: Expr -> IO ()
preprocess e = do
    (t, _) <- process e (Map.empty, Map.empty)
    return ()

process :: Expr -> ProcessState -> IO ([Type], ProcessState)
process (Seq e1 e2) (global, local) = do
    (t', (global', local')) <- process e1 (global, local)
    process e2 (global', local')

process (Literal (EInt n)) (global, local) = return ([TInt], (global, local))
process (Literal (EBool b)) (global, local) = return ([TBool], (global, local))
process (Literal Empty) (global, local) = return ([TList TEmpty], (global, local))
process (Literal ENone) (global, local) = return ([TNone], (global, local))

--process (LocalAssign (DefVar s (FuncType ps out cs)), )
--process (LocalAssign (DefVar s (Func ps e1)), )

process (LocalAssign (DefVar s e1)) (global, local) = do
    (t', (global', local')) <- process e1 (global, local)
    let lv = (lookupT s (global, local))
    if lv /= Nothing && (length (fromJust lv) /= 1 || (fromJust lv) /= t')
        then printStdErr ("WARNING: ambiguous types for: "++(show s) ++ " in " ++ (show (LocalAssign (DefVar s e1))))
        else return ()
    return (t', (global', Map.insert s t' local'))

process (Var s) state = do
    let t = (lookupT s state)
    if t == Nothing
        then do
            printStdErr ("ERROR: variable referenced before assignment: "++(show s))
            exitFailure
        else return ()
    return ((fromJust t), state)

-- Make dodgy assumption that MathOps all return ints - don't judge me josh
process (Op (MathOp op e1 e2)) (global, local)= do
    (t1, (global1, local1)) <- process e1 (global, local)
    (t2, (global2, local2)) <- process e2 (global1, local1)
    if length t1 == 1 && t1!!0 /= TInt
        then do
            printStdErr ("ERROR: type invalid for op: "++(show (Op (MathOp op e1 e2))))
            exitFailure
        else return ()
    if length t2 == 1 && t2!!0 /= TInt
        then do
            printStdErr ("ERROR: type invalid for op: "++(show (Op (MathOp op e1 e2))))
            exitFailure
        else return ()
    return ([TInt], (global2, local2))

-- TODO implement others
process e s = return ([TNone], s)

mergeTList :: [Type] -> [Type] -> [Type]
mergeTList t1 t2 = [x | x <- t1, not (x `elem` t2)] ++ t2

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



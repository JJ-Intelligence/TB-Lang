-- The Evaluator evaluates an AST, executing instructions in it.
module Evaluator where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Lazy as MapL
import Data.Maybe (fromJust)
import Data.List (sort)
import System.IO (isEOF)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Expression
import Parser
import Lexer
import Preprocessor (evaluateFuncType)

-- Reserved elements in the Store.
builtInFuncStart = 3 -- Starting address for inbuilt functions/variables.
inputStreamsToRead = 2 -- Address for input streams to read into (set using the setIn([Int]) function).
storedGlobalEnv = 1 -- Address of the global environment (needed for function calls).

-- Insert reserved items into the Environment and Store.
insertReserved :: Environment -> Store -> (Environment, Store, Address)
insertReserved env store = foldr (\(s, _, e) (env', store', a) -> (Map.insert s a env', MapL.insert a e store', a+1)) 
                                    (env, MapL.insert storedGlobalEnv (GlobalEnv Map.empty) store, builtInFuncStart) getInBuiltVars

-- Takes in a parsed Expression AST.
-- It created a start state, and then starts the 'step' finite state machine.
startEvaluator :: Expr -> IO ()
startEvaluator e = do
    s <- step (e, env, store, heapStart, [], [Done])
    return ()
        where 
            (env, store, heapStart) = insertReserved (Map.empty) (MapL.empty)

-- Step function to move from one State to another.
step :: State -> IO State

-- Converting Literals to Values.
step (Literal (EInt n), env, store, nextAddr, callS, kon) = step (Value $ VInt n, env, store, nextAddr, callS, kon)
step (Literal (EBool n), env, store, nextAddr, callS, kon) = step (Value $ VBool n, env, store, nextAddr, callS, kon)
step (Literal Empty, env, store, nextAddr, callS, kon) = step (Value $ VList (TList TEmpty) [], env, store, nextAddr, callS, kon)
step (Literal ENone, env, store, nextAddr, callS, kon) = step (Value VNone, env, store, nextAddr, callS, kon)

-- Sequence operation ';'.
step (Seq e1 e2, env, store, nextAddr, callS, kon) = step (e1, env, store, nextAddr, callS, (HBinOp $ BinSeqOp e2):kon)
step (Value e1, env, store, nextAddr, callS, (HBinOp (BinSeqOp e2)):kon) = step (e2, env, store, nextAddr, callS, kon)

-- Defining the type of a new function.
step (LocalAssign (DefVar s (ExprType (FuncType ps out cs)) _), env, store, nextAddr, callS, kon) = step (Value VNone, env', store', nextAddr', callS, kon)
    where
        (env', store', nextAddr') = updateEnvStore env store nextAddr s (VFunc ft [])
        ft = evaluateFuncType (FuncType ps out cs)

-- Defining a new Function.
step (LocalAssign (DefVar s (Func ps e1) _), env, store, nextAddr, callS, kon) = step (Value fv, env', store', nextAddr', callS, kon)
    where 
        (env', store', nextAddr') = updateEnvStore env store nextAddr s (VFuncUnTypedDef [(evaluateParams ps, e1)])
        fv = lookupVar s env' store'

        checkNoDuplicates seen [] = True
        checkNoDuplicates seen (x:xs) 
            | x `elem` seen = False
            | otherwise = checkNoDuplicates (x:seen) xs

        usedVars [] = []
        usedVars (x:xs) = (helper x) ++ (usedVars xs)
            where 
                helper (VPointer s) = [s]
                helper (VVar s) = [s]
                helper (VList t (x:xs)) = (helper x) ++ (helper (VList t xs))
                helper (VPointerList t (x:xs)) = (helper x) ++ (helper (VPointerList t xs))
                helper _ = []

        evaluateParams FuncParamEnd = []
        evaluateParams (FuncParam (PointerExpr (Op (Cons e1 e2) l)) e3) = VPointerList TParamList (evalConsParam $ Op (Cons e1 e2) l) : evaluateParams e3
        evaluateParams (FuncParam (Op (Cons e1 e2) l) e3) = VList TParamList (evalConsParam $ Op (Cons e1 e2) l) : evaluateParams e3
        evaluateParams (FuncParam e1 e2) = evalSingleParam e1 : evaluateParams e2

        evalSingleParam (Literal (EInt n)) = (VInt n)
        evalSingleParam (Literal (EBool b)) = (VBool b)
        evalSingleParam (Literal Empty) = (VList (TList TEmpty) [])
        evalSingleParam (Var s _) = (VVar s)
        evalSingleParam (PointerExpr (Var s _)) = (VPointer s)
        evalSingleParam (PointerExpr e1) = VPointerList TParamList [evalSingleParam e1]

        evalConsParam (Op (Cons e1 e2) l) = evalSingleParam e1 : evalConsParam e2
        evalConsParam e2 = [evalSingleParam e2]


-- Defining a new Var.
-- Looks for the variable in the Env, and replaces it in the Store if it exists, else it creates it.
step (LocalAssign (DefVar s e1 _), env, store, nextAddr, callS, kon) = step (e1, env, store, nextAddr, callS, (DefLocalVarFrame s):kon)
step (Value e1, env, store, nextAddr, callS, (DefLocalVarFrame s):kon) = step (Value e1, env', store', nextAddr', callS, kon)
    where (env', store', nextAddr') = updateEnvStore env store nextAddr s e1

step (GlobalAssign (DefVar s e1 _), env, store, nextAddr, callS, kon) = step (e1, env, store, nextAddr, callS, (DefGlobalVarFrame s):kon) -- TODO FIXMEEE
step (Value e1, env, store, nextAddr, callS, (DefGlobalVarFrame s):kon) = step (Value e1, env, store', nextAddr, callS, kon)
    where store' = updateGlobalEnvInStore store s e1

-- Defining a pointer variable.
step (DefPointerVar s e1, env, store, nextAddr, callS, kon) = step (e1, env, store, nextAddr, callS, (DefPointerVarFrame s):kon)
step (Value e1, env, store, nextAddr, callS, (DefPointerVarFrame s):kon) = step (Value e1, env, store', nextAddr, callS, kon)
    where val = lookupVar s env store
          (VRef r) = val
          store' = updateStore store r e1

-- Accessing a variable reference.
step (Var s _, env, store, nextAddr, callS, kon) = step (Value $ lookupVar s env store, env, store, nextAddr, callS, kon)
step (GlobalVar s _, env, store, nextAddr, callS, kon) = step (Value $ lookupVar s globalEnv store, env, store, nextAddr, callS, kon)
    where (GlobalEnv globalEnv) = fromJust $ MapL.lookup storedGlobalEnv store

-- Accessing a variable pointer.
step (PointerExpr (Var s _), env, store, nextAddr, callS, kon) = step (Value $ fromJust $ lookupPointerVar s env store, env, store, nextAddr, callS, kon)
step (PointerExpr e1, env, store, nextAddr, callS, kon) = step (e1, env, store, nextAddr, callS, kon)

-- Getting the address for an addressed variable.
step (AddressExpr (Var s _) _, env, store, nextAddr, callS, kon) = step (Value $ VRef $ lookupAddr s env, env, store, nextAddr, callS, kon)
step (AddressExpr (GlobalVar s _) _, env, store, nextAddr, callS, kon) = step (Value $ VRef $ lookupAddr s globalEnv, env, store, nextAddr, callS, kon)
    where (GlobalEnv globalEnv) = fromJust $ MapL.lookup storedGlobalEnv store

step (AddressExpr e1 _, env, store, nextAddr, callS, kon) = step (e1, env, store, nextAddr, callS, AddressExprFrame:kon)
step (Value e1, env, store, nextAddr, callS, AddressExprFrame:kon) = step (Value $ VRef $ nextAddr, env, store', nextAddr+1, callS, kon)
    where store' = updateStore store nextAddr e1


-- Function blocks ({ Expr }), which must have a 'return' statement.
step (FuncBlock e1 _, env, store, nextAddr, callS, kon) = do
    (Value e2, env', store', nextAddr', callS, kon') <- step (e1, env, store, nextAddr, callS, FuncBlockFrame:[Done])
    case head kon' of
        (ThrownException _ _) -> return (Value e2, env', store', nextAddr', callS, kon')
        _ -> step (Value e2, env', store', nextAddr', callS, kon)

step (Return e1 _, env, store, nextAddr, callS, kon) = step (e1, env, store, nextAddr, callS, ReturnFrame:kon)
step (Value e1, env, store, nextAddr, callS, FuncBlockFrame:kon) = return (Value VNone, env, store, nextAddr, callS, kon)

-- Returning from a function.
step (Value e1, env, store, nextAddr, callS, ReturnFrame:kon) = return (Value e1, env, store, nextAddr, callS, kon)

-- Try-catch statement.
step (TryCatch e1 ps e2 _, env, store, nextAddr, callS, kon) = step (e1, env, store, nextAddr, callS, (CatchFrame (evaluateCatchParams ps) e2 env callS):kon)
    where
        evaluateCatchParams :: Parameters -> [ExprValue]
        evaluateCatchParams FuncParamEnd = []
        evaluateCatchParams (FuncParam (Var x _) e2) = (lookupVar x env store) : evaluateCatchParams e2

-- User-defined function calls.
step (FuncCall s ps l, env, store, nextAddr, callS, kon) = do
    let newCallS = (FuncCall s ps l):callS
    evalArgs <- evaluateArgs ps env store nextAddr newCallS []

    case evalArgs of
        (Right (args, env', store', nextAddr')) -> do
            (matchFunc, store'') <- handleFuncArgs args env' store' nextAddr' s -- Pattern match

            case matchFunc of
                (Left ex) -> step (throwException (show ex) l, env, store, nextAddr, newCallS, kon)
                (Right (e1, env'', nextAddr'')) -> do
                    (Value e2, _, store''', _, _, funcKon) <- step (e1, env'', store'', nextAddr'', newCallS, ReturnFrame:[Done]) -- Recurse into function call.

                    case head funcKon of
                        (ThrownException ex exCallS) -> popCaseOf ex exCallS
                        _ -> step (Value e2, env', store''', nextAddr', callS, kon) -- Continue after function call returns.

        (Left (ThrownException ex exCallS)) -> popCaseOf ex exCallS

    where
        popCaseOf ex exCallS = do 
            case popKon kon ex of
                ((CatchFrame cps e2 cenv callS'):kon') -> step (e2, cenv, store, nextAddr, callS', kon') 
                (ReturnFrame:kon') -> return (Value VNone, env, store, nextAddr, callS, (ThrownException ex exCallS):[Done])
                (FuncBlockFrame:kon') -> return (Value VNone, env, store, nextAddr, callS, (ThrownException ex exCallS):[Done])
                (Done:kon') -> do
                    printStdErr $ showException ex exCallS
                    exitFailure

        popKon :: Kon -> ExprValue -> Kon
        popKon k@((CatchFrame cps e2 cenv cs):kon) v
            | v `elem` cps = k
            | otherwise = popKon kon v
        popKon k@(FuncBlockFrame:kon) v = k
        popKon k@(ReturnFrame:kon) v = k
        popKon k@(Done:kon) v = k
        popKon (_:kon) v = popKon kon v

        evaluateArgs :: Parameters -> Environment -> Store -> Address -> CallStack -> [ExprValue] -> IO (Either Frame ([ExprValue], Environment, Store, Address))
        evaluateArgs FuncParamEnd env store nextAddr callS ls = return $ Right (ls, env, store, nextAddr)
        evaluateArgs (FuncParam e1 e2) env store nextAddr callS ls = do
            (Value e1', env', store', nextAddr', exCallS, kon') <- step (e1, env, store, nextAddr, callS, [Done])
            case head kon' of
                f@(ThrownException ex exCallS) -> return $ Left f
                _ -> evaluateArgs e2 env' store' nextAddr' callS (ls ++ [e1'])


-- Built-in functions.
-- List (pass-by-value) operations:
step (BuiltInFunc "tail" [Var xs _], env, store, nextAddr, callS, kon)
    | length ys > 0 = return (Value $ VList t $ tail ys, env, store, nextAddr, callS, kon)
    | otherwise = step (throwException "EmptyListException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)
    where 
        (VList t ys) = lookupVar xs env store

step (BuiltInFunc "head" [Var xs _], env, store, nextAddr, callS, kon)
    | length ys > 0 = return (Value $ head ys, env, store, nextAddr, callS, kon)
    | otherwise = step (throwException "EmptyListException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)
    where 
        (VList t ys) = lookupVar xs env store

step (BuiltInFunc "drop" [Var n _, Var xs _], env, store, nextAddr, callS, kon) 
    | n' < 0 = step (throwException "InvalidParameterException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)
    | length ys < n' = step (throwException "IndexOutOfBoundException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)
    | otherwise = return (Value $ VList t $ drop n' ys, env, store, nextAddr, callS, kon)
    where 
        (VList t ys) = lookupVar xs env store
        (VInt n') = lookupVar n env store

step (BuiltInFunc "take" [Var n _, Var xs _], env, store, nextAddr, callS, kon) 
    | n' < 0 = step (throwException "InvalidParameterException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)
    | length ys < n' = step (throwException "IndexOutOfBoundException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)
    | otherwise = return (Value $ VList t $ take n' ys, env, store, nextAddr, callS, kon)
    where 
        (VList t ys) = lookupVar xs env store
        (VInt n') = lookupVar n env store

step (BuiltInFunc "length" [Var xs _], env, store, nextAddr, callS, kon) = return (Value $ VInt $ length ys, env, store, nextAddr, callS, kon)
    where 
        (VList t ys) = lookupVar xs env store

step (BuiltInFunc "get" [Var n _, Var xs _], env, store, nextAddr, callS, kon)
    | n' < 0 = step (throwException "InvalidParameterException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)
    | n' >= length ys = step (throwException "IndexOutOfBoundException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)
    | otherwise = return (Value $ ys!!n', env, store, nextAddr, callS, kon)
    where 
        (VList t ys) = lookupVar xs env store
        (VInt n') = lookupVar n env store


-- List/Stream reference (pass-by-reference) operations:
step (BuiltInFunc "pop" [Var p _], env, store, nextAddr, callS, kon) = case v of
        Just (VList t xs) -> case (length xs > 0) of
            True  -> return (Value $ head xs, env, updateStore store r (VList t (tail xs)), nextAddr, callS, kon)
            False -> step (throwException "EmptyListException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)

        Just (VStream i xs) -> do
            dropInp <- dropStreamInput (VStream i xs) 1 store
            case dropInp of
                (Left ex) -> step (throwException (show ex) (getFuncCallPos callS), env, store, nextAddr, callS, kon)
                (Right (ys, store')) -> 

                    case ys of
                        [VInt n] -> return (Value $ VInt n, env, store', nextAddr, callS, kon)
                        _        -> step (throwException "StreamOutOfInputException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)

    where 
        (VRef r) = lookupVar p env store
        v = MapL.lookup r store

step (BuiltInFunc "popN" [Var n _, Var p _], env, store, nextAddr, callS, kon) = case v of
        Just (VList t xs) -> case (length xs >= n') of
            True  -> return (Value $ VList t $ take n' xs, env, updateStore store r (VList t (drop n' xs)), nextAddr, callS, kon)
            False -> step (throwException "EmptyListException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)

        Just (VStream i xs) -> do
            dropInp <- dropStreamInput (VStream i xs) n' store

            case dropInp of
                (Left ex) -> step (throwException (show ex) (getFuncCallPos callS), env, store, nextAddr, callS, kon)
                (Right (ys, store')) -> 

                    case (ys == [] || VNone `elem` ys) of
                        True  -> step (throwException "StreamOutOfInputException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)
                        False ->  return (Value $ VList (TList TInt) ys, env, store', nextAddr, callS, kon)
    
    where 
        (VInt n') = lookupVar n env store
        (VRef r) = lookupVar p env store
        v = MapL.lookup r store

step (BuiltInFunc "peek" [Var p _], env, store, nextAddr, callS, kon) = case v of
        Just (VList t xs) -> case (length xs > 0) of
            True  -> return (Value $ head xs, env, store, nextAddr, callS, kon)
            False -> step (throwException "EmptyListException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)

        Just (VStream i xs) -> do
            peekInp <- peekStreamInput (VStream i xs) 1 store
            case peekInp of
                (Left ex) -> step (throwException (show ex) (getFuncCallPos callS), env, store, nextAddr, callS, kon)
                (Right (ys, store')) -> 

                    case ys of
                        [VInt n] -> return (Value $ VInt n, env, store', nextAddr, callS, kon)
                        _        -> step (throwException "StreamOutOfInputException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)

    where 
        (VRef r) = lookupVar p env store
        v = MapL.lookup r store

step (BuiltInFunc "peekN" [Var n _, Var p _], env, store, nextAddr, callS, kon) = case v of
        Just (VList t xs) -> case (length xs >= n') of
                True  -> return (Value $ VList t $ take n' xs, env, updateStore store r (VList t xs), nextAddr, callS, kon)
                False -> step (throwException "IndexOutOfBoundException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)

        Just (VStream i xs) -> do
            peekInp <- peekStreamInput (VStream i xs) n' store
            case peekInp of
                (Left ex) -> step (throwException (show ex) (getFuncCallPos callS), env, store, nextAddr, callS, kon)
                (Right (ys, store')) ->
                    
                    case (ys == [] || VNone `elem` ys) of
                        True  -> step (throwException "StreamOutOfInputException" (getFuncCallPos callS), env, store, nextAddr, callS, kon)
                        False ->  return (Value $ VList (TList TInt) ys, env, store', nextAddr, callS, kon)
    
    where 
        (VInt n') = lookupVar n env store
        (VRef r) = lookupVar p env store
        v = MapL.lookup r store

step (BuiltInFunc "isEmpty" [Var p _], env, store, nextAddr, callS, kon) = case v of
        Just (VList t xs) -> return (Value $ VBool (length xs == 0), env, store, nextAddr, callS, kon)

        Just (VStream i xs) -> do
            peekInp <- peekStreamInput (VStream i xs) 1 store
            case peekInp of
                (Left ex) -> step (throwException (show ex) (getFuncCallPos callS), env, store, nextAddr, callS, kon)
                (Right (ys, store')) -> return (Value $ VBool (length ys == 0 || VNone `elem` ys), env, store', nextAddr, callS, kon)

    where 
        (VRef r) = lookupVar p env store
        v = MapL.lookup r store

step (BuiltInFunc "hasElems" [Var n _, Var p _], env, store, nextAddr, callS, kon) = case v of
        Just (VList t xs) -> return (Value $ VBool (length xs >= n'), env, store, nextAddr, callS, kon)

        Just (VStream i xs) -> do
            peekInp <- peekStreamInput (VStream i xs) n' store
            case peekInp of
                (Left ex) -> step (throwException (show ex) (getFuncCallPos callS), env, store, nextAddr, callS, kon)
                (Right (ys, store')) -> return (Value $ VBool (length ys >= n' && not (VNone `elem` ys)), env, store', nextAddr, callS, kon)

    where 
        (VInt n') = lookupVar n env store
        (VRef r) = lookupVar p env store
        v = MapL.lookup r store

-- IO Operations:
step (BuiltInFunc "out" [Var a _], env, store, nextAddr, callS, kon) = do
    putStrLn $ (show v)
    return (Value VNone, env, store, nextAddr, callS, kon)
        where 
            v = (lookupVar a env store)

step (BuiltInFunc "in" [Var n _], env, store, nextAddr, callS, kon) = return (Value $ VRef $ -n', env, store', nextAddr, callS, kon)
    where 
        (VInt n') = (lookupVar n env store)
        store' = case MapL.lookup (-n') store of
                Just (VStream i xs) -> store
                Nothing -> updateStore store (-n') (VStream n' [])

step (BuiltInFunc "setIn" [Var xs _], env, store, nextAddr, callS, kon) = return (Value VNone, env, store', nextAddr, callS, kon)
    where 
        (VList t ys) = lookupVar xs env store
        store' = MapL.insert (inputStreamsToRead) (VList t (sort ys)) $ 
                        foldr (\(VInt x) store' -> case MapL.lookup (-x) store' of
                                Just (VStream i ys) -> store'
                                Nothing -> MapL.insert (-x) (VStream x []) store'
                        ) store ys

-- Exception operations:
step (BuiltInFunc "throw" [Var e _], env, store, nextAddr, callS, kon) = return (Value VNone, env, store, nextAddr, callS, (ThrownException v callS):kon)
    where
        v = (lookupVar e env store)

-- Boolean not operation.
step (BooleanNotExpr e1 _, env, store, nextAddr, callS, kon) = step (e1, env, store, nextAddr, callS, (BoolNotFrame):kon)
step (Value (VBool b), env, store, nextAddr, callS, (BoolNotFrame):kon) = step (Value $ VBool $ not b, env, store, nextAddr, callS, kon)

-- Math binary operations.
step (Op (MathOp op e1 e2) _, env, store, nextAddr, callS, kon) = step (e1, env, store, nextAddr, callS, (HBinOp $ BinMathOp op e2 env):kon)
step (Value e1, env, store, nextAddr, callS, (HBinOp (BinMathOp op e2 env')):kon) = step (e2, env', store, nextAddr, callS, (BinOpH $ BinMathOp op (Value e1) env):kon)

step (Value (VInt n'), env', store, nextAddr, callS, (BinOpH (BinMathOp op (Value (VInt n)) env)):kon) = step (Value $ VInt r, env, store, nextAddr, callS, kon)
    where r = case op of
                    Plus -> n + n'
                    Min -> n - n'
                    Mult -> n * n'
                    Div -> n `div` n'
                    Exp -> n ^ n' 
                    Mod -> n `mod` n'

-- Adding 2 lists together.
step (Value (VList t' n'), env', store, nextAddr, callS, (BinOpH (BinMathOp Plus (Value (VList t n)) env)):kon) = step (Value $ VList newT (n ++ n'), env, store, nextAddr, callS, kon)
    where
        newT = joinListTypes t t'

        joinListTypes TInt TInt = TInt
        joinListTypes TBool TBool = TBool
        joinListTypes t TEmpty = t
        joinListTypes TEmpty t = t
        joinListTypes TNone TNone = TNone
        joinListTypes (TRef t) (TRef t')
            | newT /= TConflict = TRef newT
            | otherwise = TConflict
            where
                newT = joinListTypes t t'

        joinListTypes (TList t) (TList t')
            | newT /= TConflict = TList newT
            | otherwise = TConflict
            where
                newT = joinListTypes t t'

        joinListTypes _ _ = TConflict


-- Binary comparison operations.
step (Op (CompOp op e1 e2) _, env, store, nextAddr, callS, kon) = step (e1, env, store, nextAddr, callS, (HBinOp $ BinCompOp op e2 env):kon)
step (Value e1, env, store, nextAddr, callS, (HBinOp (BinCompOp op e2 env')):kon) = step (e2, env', store, nextAddr, callS, (BinOpH $ BinCompOp op (Value e1) env):kon)

-- Boolean &&, || operations.
step (Value (VBool b'), env', store, nextAddr, callS, (BinOpH (BinCompOp And (Value (VBool b)) env)):kon)
    | b' = step (Value $ VBool $ b && b', env, store, nextAddr, callS, kon)
    | otherwise = step (Value $ VBool False, env, store, nextAddr, callS, kon)

step (Value (VBool b'), env', store, nextAddr, callS, (BinOpH (BinCompOp Or (Value (VBool b)) env)):kon) 
    | b' = step (Value $ VBool True, env, store, nextAddr, callS, kon)
    | otherwise = step (Value $ VBool $ b || b', env, store, nextAddr, callS, kon)

-- Comparison <, >, == operations.
step (Value e2, env', store, nextAddr, callS, (BinOpH (BinCompOp LessThan (Value e1) env)):kon) = step (Value $ VBool $ e1 < e2, env, store, nextAddr, callS, kon)
step (Value e2, env', store, nextAddr, callS, (BinOpH (BinCompOp GreaterThan (Value e1) env)):kon) = step (Value $ VBool $ e1 > e2, env, store, nextAddr, callS, kon)
step (Value e2, env', store, nextAddr, callS, (BinOpH (BinCompOp Equality (Value e1) env)):kon) = step (Value $ VBool $ e1 == e2, env, store, nextAddr, callS, kon)
step (Value e2, env', store, nextAddr, callS, (BinOpH (BinCompOp NotEquals (Value e1) env)):kon) = step (Value $ VBool $ e1 /= e2, env, store, nextAddr, callS, kon)

-- Cons operation.
step (Op (Cons e1 e2) _, env, store, nextAddr, callS, kon) = step (e1, env, store, nextAddr, callS, (HBinOp $ BinConsOp e2 env):kon)
step (Value e1, env, store, nextAddr, callS, (HBinOp (BinConsOp e2 env')):kon) = step (e2, env', store, nextAddr, callS, (BinOpH (BinConsOp (Value e1) env')):kon)
step (Value (VList t xs), env, store, nextAddr, callS, (BinOpH (BinConsOp (Value e1) env')):kon) = step (Value (VList t' (e1:xs)), env', store, nextAddr, callS, kon)
    where 
        t' = consTypes (getType store e1) t

        consTypes e1 (TList e2) 
            | e1 == e2 = TList e1
            | isEmptyList e2 = TList $ buildList e1 e2
            where
                buildList e1 TEmpty = e1
                buildList e1 (TList e2) = TList (buildList e1 e2)

                isEmptyList TEmpty = True
                isEmptyList (TList e1) = isEmptyList e1
                isEmptyList _ = False

        consTypes (TList e1) (TList e2) = TList (consTypes e1 e2)
        consTypes TEmpty e@(TList e2) = e
        
        consTypes _ _ = TConflict


-- if-elif-else statement.
step (If c e1 e2 _, env, store, nextAddr, callS, kon) = step (c, env, store, nextAddr, callS, (HTerOp $ TerIfOp e1 e2):kon)
step (Value (VBool b), env, store, nextAddr, callS, (HTerOp (TerIfOp e1 e2)):kon)
    | b = step (e1, env, store, nextAddr, callS, kon)
    | otherwise = step $ helper e2
    where helper Nothing = (Value VNone, env, store, nextAddr, callS, kon)
          helper (Just (Else e)) = (e, env, store, nextAddr, callS, kon)
          helper (Just (Elif c' e1' e2' l)) = (If c' e1' e2' l, env, store, nextAddr, callS, kon)


-- While loop.
step (While c e1 _, env, store, nextAddr, callS, kon) = step (c, env, store, nextAddr, callS, (HTerOp $ TerWhileOp c e1):kon)
step (Value (VBool b), env, store, nextAddr, callS, (HTerOp (TerWhileOp c e1)):kon)
    | b = step (e1, env, store, nextAddr, callS, (TerOpH $ TerWhileOp c e1):kon)
    | otherwise = step (Value VNone, env, store, nextAddr, callS, kon)
step (Value v, env, store, nextAddr, callS, (TerOpH (TerWhileOp c e1)):kon) = step (c, env, store, nextAddr, callS, (HTerOp $ TerWhileOp c e1):kon)


-- For loop.
step (For i c n e _, env, store, nextAddr, callS, kon) = step (i, env, store, nextAddr, callS, (HTerOp (TerForInit i c n e)):kon)
step (Value _, env, store, nextAddr, callS, (HTerOp (TerForInit i c n e)):kon) = step (c, env, store, nextAddr, callS, (HTerOp (TerForOp c n e)):kon) -- Initialised vars.
step (Value (VBool b), env, store, nextAddr, callS, (HTerOp (TerForOp c n e)):kon) -- Evaluated condition.
    | b = step (e, env, store, nextAddr, callS, (TerOp_H (TerForOp c n e)):kon)
    | otherwise = step (Value VNone, env, store, nextAddr, callS, kon)
step (Value _, env, store, nextAddr, callS, (TerOp_H (TerForOp c n e)):kon) = step (n, env, store, nextAddr, callS, (TerOpH (TerForOp c n e)):kon) -- Evaluated expression
step (Value _, env, store, nextAddr, callS, (TerOpH (TerForOp c n e)):kon) = step (c, env, store, nextAddr, callS, (HTerOp (TerForOp c n e)):kon) -- Incremented vars.


-- End of try block with no exception thrown.
step (e, env, store, a, _, (CatchFrame _ _ _ callS):kon) = step (e, env, store, a, callS, kon)

-- End of evaluation.
step s@(_, _, _, _, _, [Done]) = return s

-- No defined step for the current State.
step s@(exp, env, store, nextAddr, callS, kon) = do
    printStdErr $ "Evaluation ERROR: This expression could not be evaluated \'" ++ (show exp) ++ "\'"
    exitFailure

-- Takes in a list of evaluated arguments for a function call; the current environment; the current store; 
-- the current env/store address; the function name.
-- It pattern-matches a functions parameters with the given arguments, returning the functions value and 
-- an updated environment, store and next address.
handleFuncArgs :: [ExprValue] -> Environment -> Store -> Address -> String -> IO (Either Exception (Expr, Environment, Address), Store)
handleFuncArgs args env store nextAddr s = do
    let fv@(VFunc (TFunc ts _ cs) _) = (lookupVar s env store)
    (matchFunc, store') <- matchArgsToFunc store args fv -- Match the arguments to the functions' definitions.

    case matchFunc of
        (Left ex) -> return (Left ex, store')
        (Right (e1, xs)) -> do
            let (store'') = let (Just (GlobalEnv e)) = MapL.lookup storedGlobalEnv store' in if (e == Map.empty) 
                    then (MapL.update (\x -> Just $ GlobalEnv env) storedGlobalEnv store') 
                    else (store') -- lookup and update the global environment in the Store.
            let (env', store''', nextAddr') = 
                    foldl (\(accEnv, accStore, addr) (s, e2) -> (overrideEnvStore accEnv accStore addr s e2)) (Map.empty, store'', nextAddr) xs

            return (Right (e1, env', nextAddr'), store''')

-- Takes in the Store; a list of evaluated arguments; the list of function definitions from the Store.
-- Returns an updated Store, and either an exception or the function expression to use and a list of variables to expression 
-- values which need to be put into the Store (bound parameters).
matchArgsToFunc :: Store -> [ExprValue] -> ExprValue -> IO (Either Exception (Expr, [(String, ExprValue)]), Store)
matchArgsToFunc store _ (VFunc t []) = return $ (Left NonExhaustivePatternException, store)
matchArgsToFunc store args (VFunc t ((ps,e1):xs)) = do
     -- Fold over the parameters and arguments together, trying to pattern-match them.
    (res, store') <- foldr (\(p, a) eith -> do
                                acc <- eith
                                case acc of
                                    (Left ex, store') -> return $ (Left ex, store)
                                    (Right ls, store') ->
                                        case ls of
                                            Nothing -> return $ (Right Nothing, store')
                                            Just (ls') -> matchParamToArg p a ls' store'
                            ) (return (Right $ Just [], store)) (zip ps args)

    case res of
        (Left ex) -> return (Left ex, store')
        (Right ys) -> 
            case length args /= length ps  || ys == Nothing of
                True -> matchArgsToFunc store' args (VFunc t xs)
                False -> return $ (Right (e1, fromJust ys), store')

        where
            -- Pattern match a single parameter to its corresponding argument.
            -- Takes in the evaluated parameter, evaluated argument, the current list of variable bindings, and the Store.
            -- Returns the updated Store and either an exception or the updated list of variable bindings.
            matchParamToArg :: ExprValue -> ExprValue -> [(String, ExprValue)] -> Store -> IO (Either Exception (Maybe [(String, ExprValue)]), Store)
            matchParamToArg (VList _ []) (VList _ []) ls store = return $ (Right $ Just ls, store)
            matchParamToArg (VList _ [VList _ []]) (VList _ []) ls store = return (Right $ Just ls, store)
            matchParamToArg (VList _ [VVar s]) (VList t ys) ls store = matchParamToArg (VVar s) (VList t ys) ls store

            matchParamToArg (VList t (x:xs)) (VList t' (y:ys)) ls store = do
                (matchParam, store') <- matchParamToArg x y ls store
                case matchParam of
                    (Left ex) -> return $ (Left ex, store')
                    (Right e) -> 
                        case e of
                            Nothing -> return $ (Right Nothing, store')
                            Just (ls') -> matchParamToArg (VList t xs) (VList t' ys) ls' store'

            matchParamToArg l@(VPointerList t xs) r@(VRef a) ls store
                | v /= Nothing && ((isTList $ getType store v') || getType store v' == TStream) = matchPointerListToRef l r v' ls store 0
                | otherwise = return (Right Nothing, store)
                where v = MapL.lookup a store
                      (Just v') = v

                      isTList (TList _) = True
                      isTList _ = False

            matchParamToArg (VPointer s) e2@(VRef a) ls store = return (Right $ Just ((s, e2):ls), store)
            matchParamToArg _ e2@(VRef a) ls store = return (Right Nothing, store)
            matchParamToArg (VVar s) e2 ls store = return (Right $ Just ((s, e2):ls), store)
            matchParamToArg (VInt n) (VInt n') ls store = return (Right $ if n == n' then Just ls else Nothing, store)
            matchParamToArg (VBool b) (VBool b') ls store = return (Right $ if b == b' then Just ls else Nothing, store)
            matchParamToArg _ _ _ store = return (Right Nothing, store)

            -- Pattern-matches a pointer list parameter (e.g. *(x:y:xs)) to its corresponding argument.
            matchPointerListToRef :: ExprValue -> ExprValue -> ExprValue -> [(String, ExprValue)] -> Store -> Int -> IO (Either Exception (Maybe [(String, ExprValue)]), Store)
            matchPointerListToRef (VPointerList t []) _ _ ls store c = return (Right $ Just ls, store)
            matchPointerListToRef (VPointerList _ [VList _ []]) r (VList _ []) ls store c = return (Right $ Just ls, store)
            matchPointerListToRef (VPointerList _ [VList _ []]) r st@(VStream _ _) ls store c = do
                peekInp <- peekStreamInput st (c+1) store
                case peekInp of
                    (Left ex) -> return $ (Left ex, store)
                    (Right (xs, store')) -> 

                        case ((length xs)-1 < c || xs!!c /= VNone) of
                            True -> return (Right Nothing, store')
                            False -> do

                                dropInp <- dropStreamInput st c store'
                                case dropInp of
                                    (Left ex) -> return $ (Left ex, store')
                                    (Right (_, store'')) -> return (Right $ Just ls, store'')

            matchPointerListToRef (VPointerList _ [VVar s]) r@(VRef a) e@(VList t ys) ls store c = 
                return (Right $ Just ((s,r):ls), MapL.update (\x -> Just $ e) a store)

            matchPointerListToRef (VPointerList _ [VVar s]) r@(VRef a) st@(VStream _ _) ls store c = do
                dropInp <- dropStreamInput st c store
                case dropInp of
                    (Left ex) -> return $ (Left ex, store)
                    (Right (_, store')) -> return (Right $ Just ((s,r):ls), store')

            matchPointerListToRef (VPointerList t (x:xs)) r (VList t' (y:ys)) ls store c = do
                (matchParam, store') <- matchParamToArg x y ls store
                case matchParam of
                    (Left ex) -> return $ (Left ex, store')
                    (Right e) ->

                        case e of
                            Nothing -> return (Right Nothing, store')
                            Just (ls') -> matchPointerListToRef (VPointerList t xs) r (VList t' ys) ls' store' c

            matchPointerListToRef (VPointerList t (x:xs)) r st@(VStream i _) ls store c = do
                peekInp <- peekStreamInput st (c+1) store
                case peekInp of
                    (Left ex) -> return $ (Left ex, store)
                    (Right (ys, store')) -> do

                        case (length ys)-1 < c of
                            True -> return (Right Nothing, store')
                            False -> 

                                case ys!!c of
                                    VNone -> return (Right Nothing, store')
                                    y -> do

                                        (matchParam, store'') <- matchParamToArg x y ls store'
                                        case matchParam of
                                            (Left ex) -> return $ (Left ex, store'')
                                            (Right e) ->

                                                case e of
                                                    Nothing -> return (Right Nothing, store'')
                                                    Just (ls') -> matchPointerListToRef (VPointerList t xs) r st ls' store'' (c+1)

            matchPointerListToRef _ _ _ _ store _ = return (Right Nothing, store)


-- Gets the type of a evaluated expression.
getType :: Store -> ExprValue -> Type
getType store (VInt _) = TInt
getType store (VBool _) = TBool
getType store (VStream _ _) = TStream
getType store VNone = TNone

getType store (VRef r)
    | v /= Nothing = TRef $ getType store (fromJust v)
    | otherwise = TConflict
    where
        v = MapL.lookup r store

getType store (VList TParamList xs)
    | t == TConflict = TConflict
    | otherwise = TList t
    where
        t = evaluateListType store xs TParamList

getType store (VPointerList TParamList xs)
    | t == TConflict = TConflict
    | otherwise = TRef t
    where
        t = evaluateListType store xs TParamList

getType store (VList t xs) = t
getType store (VPointerList t xs) = t
getType store (VFunc t xs) = t
getType store (VException e) = TException
getType _ e = error (show e)

-- Evaluates the type of a parameter list (used for pattern-matching).
evaluateListType :: Store -> [ExprValue] -> Type -> Type -- Used for checking list types of parameter lists
evaluateListType store [] t = t
evaluateListType store ((VInt _):xs) TInt = evaluateListType store xs TInt
evaluateListType store ((VBool _):xs) TBool = evaluateListType store xs TBool
evaluateListType store ((VPointer _):xs) t@(TRef r) = evaluateListType store xs t
evaluateListType store ((VVar _):xs) t = evaluateListType store xs t
evaluateListType store (VNone:xs) TNone = evaluateListType store xs TNone
evaluateListType store (x:xs) TParamList = evaluateListType store xs (getType store x)

evaluateListType store ((VFunc t ds):xs) t'
    | t == t' = evaluateListType store xs t'
    | otherwise = TConflict

evaluateListType store ((VList t ys):xs) t'
    | getType store (VList t ys) == (TList t') = evaluateListType store xs t'
    | otherwise = TConflict

evaluateListType store ((VPointerList t ys):xs) t'
    | getType store (VPointerList t ys) == (TRef t') = evaluateListType store xs t'
    | otherwise = TConflict


evaluateListType _ (_:xs) _ = TConflict

-- Lookup a variable in the Environment (if not in local env, then search global env) and Store, returning its value.
lookupVar :: String -> Environment -> Store -> ExprValue
lookupVar s env store
    | localAddr /= Nothing = fromJust $ MapL.lookup (fromJust localAddr) store
    | globalAddr /= Nothing = fromJust $ MapL.lookup (fromJust globalAddr) store
    | otherwise =  error $ "Value " ++ s ++ " is not in the Environment (has not been defined) -> " ++ (show env)
    where localAddr = Map.lookup s env
          globalAddr = Map.lookup s globalEnv
          (GlobalEnv globalEnv) = fromJust $ MapL.lookup storedGlobalEnv store

-- Looks up a pointer variable in the Environment and Store, returning its value.
lookupPointerVar :: String -> Environment -> Store -> Maybe ExprValue
lookupPointerVar s env store
    | (isTRef $ getType store val) = MapL.lookup r store
    | otherwise = Nothing
    where val = lookupVar s env store
          (VRef r) = val

          isTRef (TRef _) = True
          isTRef _ = False

-- Looks up the address of a variable in the Environment.
lookupAddr :: String -> Environment -> Address
lookupAddr s env
    | addr == Nothing = error $ "Value " ++ s ++ " is not in the Environment (has not been defined) -> " ++ (show env)
    | otherwise = fromJust addr
    where addr = Map.lookup s env

-- Update a global environment which is stored in the Store, e.g. by doing 'global x = 4'.
updateGlobalEnvInStore :: Store -> String -> ExprValue -> Store
updateGlobalEnvInStore store s e1
    | globalVar == Nothing = error "Variable could not be found in the global scope/environment."
    | otherwise = MapL.update (\x -> Just e1) (fromJust globalVar) store
    where 
        (GlobalEnv globalEnv) = fromJust $ MapL.lookup storedGlobalEnv store
        globalVar = Map.lookup s globalEnv


-- Binds a String to an expression, overriding the String address and scope if it already exists in the Environment.
overrideEnvStore :: Environment -> Store -> Address -> String -> ExprValue -> (Environment, Store, Address)
overrideEnvStore env store nextAddr s e1 = (Map.insert s nextAddr env, updateStore store nextAddr e1, nextAddr+1)

-- Binds a String (variable name) to an expression, updating the environment and store and returning them.
updateEnvStore :: Environment -> Store -> Address -> String -> ExprValue -> (Environment, Store, Address)
updateEnvStore env store nextAddr s e1 = (env', updateStore store addr e1, if lookupInEnv == Nothing then nextAddr+1 else nextAddr)
    where 
        lookupInEnv = Map.lookup s env
        (env', addr) = case lookupInEnv of
            Just (a) -> (env, a)
            Nothing -> addToEnv env store nextAddr s

-- Adds a new String to the Environment, and returns a tuple of the new Environment and the created Address.
-- Assumes the String is not in the Environment.
addToEnv :: Environment -> Store -> Address -> String -> (Environment, Address)
addToEnv env store nextAddr s = (Map.insert s addr env, addr)
    where 
        addr = case Map.lookup s env of 
            Just (a) -> a
            Nothing -> nextAddr

-- Updates an Address mapping in the store.
-- If the inputted Address is not in the store, then it will be inserted.
updateStore :: Store -> Address -> ExprValue -> Store
updateStore store a e@(VFunc ft []) = MapL.insert a e store
updateStore store a (VFuncUnTypedDef xs) = MapL.update (\(VFunc (TFunc ts out cs) ys) -> Just (VFunc (TFunc ts out cs) (ys++xs))) a store
updateStore store a e = MapL.insert a e store

-- Takes in stream num, num of values to get from the buffer, and the current store.
-- Returns updated store and list of values retrieved.
peekStreamInput :: ExprValue -> Int -> Store -> IO (Either Exception ([ExprValue], Store))
peekStreamInput st@(VStream i _) n store = do
    inp <- handleStreamInput st n store
    case inp of
        (Left ex) -> return $ Left ex
        (Right (VStream i' ys, store')) -> return $ Right (take n ys, store')
peekStreamInput _ _ _ = error "peekStreamInput function only takes in a VStream type."

-- Takes in stream num, num of values to get from the buffer, and the current store.
-- Returns updated store and list of values retrieved.
dropStreamInput :: ExprValue -> Int -> Store -> IO (Either Exception ([ExprValue], Store))
dropStreamInput st@(VStream i _) n store = do
    inp <- handleStreamInput st n store
    case inp of
        (Left ex) -> return $ Left ex
        (Right (VStream i' ys, store')) -> return $ Right (take n ys, MapL.update (\x -> Just (VStream i' $ drop n ys)) (-i') store')
dropStreamInput _ _ _ = error "dropStreamInput function only takes in a VStream type."

handleStreamInput :: ExprValue -> Int -> Store -> IO (Either Exception (ExprValue, Store)) -- Update to just take in address!
handleStreamInput (VStream i _) n store = do
    let (VStream i' xs) = fromJust $ MapL.lookup (-i) store

    case length xs >= n of
        True -> return $ Right (VStream i' xs, store)
        False -> do
            upStr <- updateStreams (n - length xs) store
            
            case upStr of 
                (Left ex) -> return $ Left ex
                (Right store') -> return $ Right (fromJust $ MapL.lookup (-i) store', store')

-- Input function. Reads in n values from a given sequence.
updateStreams :: Int -> Store -> IO (Either Exception Store)
updateStreams n store = do
    inp <- readInput [] (if n > 100 then n else 100)

    case inp of
        (Left e) -> return $ Left e
        (Right xss) -> do
            let ks = case MapL.lookup inputStreamsToRead store of
                            Just (VList t ks') -> foldr (\(VInt k) acc -> k:acc) [] ks'
                            Nothing -> []

            case xss of
                [] -> case ks of
                    [] -> return $ Right $ helper 0 store
                    _ -> return $ Right $ foldl (\store' k -> MapL.update (\(VStream i ys) -> Just (VStream i (ys++[VNone]))) (-k) store') store ks
                _ -> case ks of
                    [] -> return $ Right $ fst $ foldl (\(store', c) xs -> case MapL.lookup c store' of
                                                Just (VStream i ys) -> (MapL.update (\x -> Just (VStream i (ys++xs))) c store', c-1)
                                                Nothing -> (MapL.insert c (VStream (abs c) xs) store', c-1)
                                ) (store, 0) xss
                    _ -> case (length xss)-1 < (last ks) of
                        False -> return $ Right $ foldl (\store' k -> MapL.update (\(VStream i ys) -> Just (VStream i (ys++(xss!!k)))) (-k) store') store ks
                        True -> return $ Left StreamOutOfInputException

    where 
        helper c store = case MapL.lookup c store of
            Just (VStream i ys) -> helper (c-1) (MapL.update (\x -> Just (VStream i (ys++[VNone]))) c store)
            Nothing -> store

-- Read n lines of input into stream buffers (a list of lists).
readInput :: [[ExprValue]] -> Int -> IO (Either (Exception) [[ExprValue]])
readInput xss 0 = return $ Right $ map reverse xss
readInput [] n = do
    end <- isEOF
    case end of 
        True -> return $ Right []
        False -> do
            line <- getLine

            case helper (words line) of
                (Left e) -> return $ Left e
                (Right xss') -> readInput xss' (n-1)

    where
        helper :: [String] -> Either Exception [[ExprValue]]
        helper [] = Right []
        helper (y:ys)
            | res == [] || (snd $ head res) /= "" = Left InvalidInputException
            | otherwise = case helper ys of
                (Left e) -> Left e
                (Right xss') -> Right $ [VInt $ fst $ head res]:xss'
            where 
                res = (reads y) :: [(Int, String)]

readInput xss n = do
    end <- isEOF
    case end of
        True -> return $ Right $ map (reverse . (VNone:)) xss
        False -> do
            line <- getLine
            let res = helper xss (words line)

            case res of
                (Left e) -> return $ Left e
                (Right xss') -> readInput xss' (n-1)

    where
        helper :: [[ExprValue]] -> [String] -> Either Exception [[ExprValue]]
        helper [] [] = Right $ []
        helper [] _ = Left InvalidInputException
        helper _ [] = Left InvalidInputException
        helper (xs:xss) (y:ys)
            | res == [] || (snd $ head res) /= "" = Left InvalidInputException
            | otherwise = case helper xss ys of
                (Left e) -> Left e
                (Right xss') -> Right $ ((VInt $ fst $ head res):xs):xss'
            where
                res = (reads y) :: [(Int, String)]

-- Generates a function call for throwing an exception s.
throwException :: String -> Pos -> Expr
throwException s l = FuncCall "throw" (FuncParam (Var s (0,0)) FuncParamEnd) l

-- Shows an exception and its call stack, to then be printed out.
showException :: ExprValue -> CallStack -> String
showException ex exCallS = (show ex) ++ " in evaluation:" ++ showCS exCallS
    where
        showCS [] = ""
        showCS (c:cs) = "\n\tIn " ++ case c of
            (FuncCall s ps l) -> "function call " ++ (show c) ++ (printPos l) ++ (showCS cs) 
            _ -> (show c) ++ (showCS cs)

-- Gets the posiiton of the top elemnt of a function call stack.
getFuncCallPos :: CallStack -> Pos
getFuncCallPos ((FuncCall _ _ l):_) = l
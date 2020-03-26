module Evaluator where
import Expression
import Parser
import Lexer
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Lazy as MapL
import Data.Maybe
import Data.List
import System.IO (isEOF)
import Debug.Trace

-- Reserved elements of the Store.
builtInFuncStart = 3 -- Starting address for built in functions in the env/store.
inputStreamsToRead = 2
storedGlobalEnv = 1 -- Address of the function CallStack.

-- Insert reserved items into the Environment and Store.
insertReserved :: Environment -> Store -> (Environment, Store, Address)
insertReserved env store = helper ls env (MapL.insert storedGlobalEnv (GlobalEnv Map.empty) store)
    where ls = [("tail", (VFunc 
                    (TFunc [TList $ TGeneric "a"] (TList $ TGeneric "a") []) 
                    [([VVar "xs"], BuiltInFunc "tail" [Var "xs"])])),
                ("head", (VFunc 
                    (TFunc [TList $ TGeneric "a"] (TGeneric "a") []) 
                    [([VVar "xs"], BuiltInFunc "head" [Var "xs"])])),
                ("drop", (VFunc 
                    (TFunc [TInt, TList $ TGeneric "a"] (TList $ TGeneric "a") []) 
                    [([VVar "n", VVar "xs"], BuiltInFunc "drop" [Var "n", Var "xs"])])),
                ("take", (VFunc 
                    (TFunc [TInt, TList $ TGeneric "a"] (TList $ TGeneric "a") []) 
                    [([VVar "n", VVar "xs"], BuiltInFunc "take" [Var "n", Var "xs"])])),
                ("length", (VFunc 
                    (TFunc [TList $ TGeneric "a"] (TInt) []) 
                    [([VVar "xs"], BuiltInFunc "length" [Var "xs"])])),
                ("get", (VFunc 
                    (TFunc [TInt, TList $ TGeneric "a"] (TGeneric "a") []) 
                    [([VVar "n", VVar "xs"], BuiltInFunc "get" [Var "n", Var "xs"])])),
                ("out", (VFunc 
                    (TFunc [TGeneric "a"] (TNone) []) 
                    [([VVar "v"], BuiltInFunc "out" [Var "v"])])),
                ("in", (VFunc 
                    (TFunc [TInt] (TRef $ TStream) []) 
                    [([VVar "v"], BuiltInFunc "in" [Var "v"])])),
                ("setIn", (VFunc 
                    (TFunc [TList $ TInt] (TNone) []) 
                    [([VVar "xs"], BuiltInFunc "setIn" [Var "xs"])])),
                ("pop", (VFunc 
                    (TFunc [TRef $ TIterable $ TGeneric "a"] (TGeneric "a") []) 
                    [([VVar "xs"], BuiltInFunc "pop" [Var "xs"])])),
                ("popN", (VFunc 
                    (TFunc [TInt, TRef $ TIterable $ TGeneric "a"] (TList $ TGeneric "a") []) 
                    [([VVar "n", VVar "xs"], BuiltInFunc "popN" [Var "n", Var "xs"])])),
                ("peek", (VFunc 
                    (TFunc [TRef $ TIterable $ TGeneric "a"] (TGeneric "a") []) 
                    [([VVar "xs"], BuiltInFunc "peek" [Var "xs"])])),
                ("peekN", (VFunc 
                    (TFunc [TInt, TRef $ TIterable $ TGeneric "a"] (TList $ TGeneric "a") []) 
                    [([VVar "n", VVar "xs"], BuiltInFunc "peekN" [Var "n", Var "xs"])])),
                ("isEmpty", (VFunc 
                    (TFunc [TRef $ TIterable $ TGeneric "a"] (TBool) []) 
                    [([VVar "xs"], BuiltInFunc "isEmpty" [Var "xs"])])),
                ("hasElems", (VFunc 
                    (TFunc [TInt, TRef $ TIterable $ TGeneric "a"] (TBool) []) 
                    [([VVar "n", VVar "xs"], BuiltInFunc "hasElems" [Var "n", Var "xs"])]))]
          helper xs env store = foldr (\(s,e) (env', store', a) -> (Map.insert s a env', MapL.insert a e store', a+1)) (env, store, builtInFuncStart) xs

-- interpret :: String -> State
-- interpret s = eval $ parse $ alexScanTokens s

-- compiler :: Expr -> State
-- compiler e = (e, env, store, heapStart, [Done])

-- Start the evaluator by passing it an Expression (from the Parser).
startEvaluator :: Expr -> IO ()
startEvaluator e = do
    s <- step (e, env, store, heapStart, [Done])
    putStrLn $ "\nFinished evaluation.\n" ++ (show s)
        where 
            (env, store, heapStart) = insertReserved (Map.empty) (MapL.empty)

-- Step function to move from one State to another.
step :: State -> IO State

-- Converting Literals to Values.
step (Literal (EInt n), env, store, nextAddr, kon) = step (Value $ VInt n, env, store, nextAddr, kon)
step (Literal (EBool n), env, store, nextAddr, kon) = step (Value $ VBool n, env, store, nextAddr, kon)
step (Literal Empty, env, store, nextAddr, kon) = step (Value $ VList (TList TEmpty) [], env, store, nextAddr, kon)
step (Literal ENone, env, store, nextAddr, kon) = step (Value VNone, env, store, nextAddr, kon)

-- Sequence operation ';'.
step (Seq e1 e2, env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (HBinOp $ BinSeqOp e2):kon)
step (Value e1, env, store, nextAddr, (HBinOp (BinSeqOp e2)):kon) = step (e2, env, store, nextAddr, kon)

-- Defining the type of a new function.
step (LocalAssign (DefVar s (FuncType ps out cs)), env, store, nextAddr, kon) = step (Value VNone, env', store', nextAddr', kon)
    where
        (env', store', nextAddr') = updateEnvStore env store nextAddr s (VFunc ft [])
        ft = buildTFunc ps out cs

        evaluateParams FuncParamEnd = []
        evaluateParams (FuncParam (FuncType ps' out' cs') e3) = buildTFunc ps' out' cs' : evaluateParams e3
        evaluateParams (FuncParam (ExprType t) e3) = t : evaluateParams e3

        evaluateOut (ExprType t) = t
        evaluateOut (FuncType ps' out' cs') = buildTFunc ps' out' cs'

        evaluateConstraints FuncParamEnd = []
        evaluateConstraints (FuncParam (TypeConstraint cl g) e3) = (g, cl) : evaluateConstraints e3

        buildTFunc ps out cs = TFunc (evaluateParams ps) (evaluateOut out) (if cs == Nothing then [] else evaluateConstraints $ fromJust cs)

-- Defining a new Function.
step (LocalAssign (DefVar s (Func ps e1)), env, store, nextAddr, kon)
    | not (validType store cs ts params) = error $ "Type error in defining function, function type doesn't match defined type: " ++ (show $ (DefVar s (Func ps e1)))
    | checkNoDuplicates [] $ usedVars params = step (Value fv, env', store', nextAddr', kon)
    | otherwise = error $ "Function '" ++ s ++"' has parameters containing multiple variables of the same name. \nIn " ++ (show $ Func ps e1)
    where 
        fv = lookupVar s env' store'
        (VFunc (TFunc ts _ cs) _) = fv
        (env', store', nextAddr') = updateEnvStore env store nextAddr s (VFuncUnTypedDef fdefs)
        fdefs = [(params, e1)]
        params = evaluateParams ps

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
        evaluateParams (FuncParam (PointerExpr (Op (Cons e1 e2))) e3) = VPointerList TParamList (evalConsParam $ Op (Cons e1 e2)) : evaluateParams e3
        evaluateParams (FuncParam (Op (Cons e1 e2)) e3) = VList TParamList (evalConsParam $ Op (Cons e1 e2)) : evaluateParams e3
        evaluateParams (FuncParam e1 e2) = evalSingleParam e1 : evaluateParams e2

        evalSingleParam (Literal (EInt n)) = (VInt n)
        evalSingleParam (Literal (EBool b)) = (VBool b)
        evalSingleParam (Literal Empty) = (VList (TList TEmpty) [])
        evalSingleParam (Var s) = (VVar s)
        evalSingleParam (PointerExpr (Var s)) = (VPointer s)
        evalSingleParam (PointerExpr e1) = VPointerList TParamList [evalSingleParam e1]

        evalConsParam (Op (Cons e1 e2)) = evalSingleParam e1 : evalConsParam e2
        evalConsParam e2 = [evalSingleParam e2]


-- Defining a new Var.
-- Looks for the variable in the Env, and replaces it in the Store if it exists, else it creates it.
step (LocalAssign (DefVar s e1), env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (DefLocalVarFrame s env):kon)
step (Value e1, env, store, nextAddr, (DefLocalVarFrame s env'):kon) = step (Value e1, env'', store', nextAddr', kon)
    where (env'', store', nextAddr') = updateEnvStore env' store nextAddr s e1

step (GlobalAssign (DefVar s e1), env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (DefGlobalVarFrame s env):kon) -- TODO FIXMEEE
step (Value e1, env, store, nextAddr, (DefGlobalVarFrame s env'):kon) = step (Value e1, env', store', nextAddr, kon)
    where store' = updateGlobalEnvInStore store s e1

-- Defining a pointer variable.
step (DefPointerVar s e1, env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (DefPointerVarFrame s env):kon)
step (Value e1, env, store, nextAddr, (DefPointerVarFrame s env'):kon)
    | (isTRef val) = step (Value e1, env', store', nextAddr, kon)
    | otherwise = error "Pointer is not a reference!"
    where val = lookupVar s env store
          (VRef r) = val
          store' = updateStore store r e1

          isTRef (VRef _) = True
          isTRef _ = False

-- Accessing a variable reference.
step (Var s, env, store, nextAddr, kon) = step (Value $ lookupVar s env store, env, store, nextAddr, kon)
step (GlobalVar s, env, store, nextAddr, kon) = step (Value $ lookupVar s globalEnv store, env, store, nextAddr, kon)
    where (GlobalEnv globalEnv) = fromJust $ MapL.lookup storedGlobalEnv store

-- Accessing a variable pointer.
step (PointerExpr (Var s), env, store, nextAddr, kon) = step (Value $ lookupPointerVar s env store, env, store, nextAddr, kon)
step (PointerExpr e1, env, store, nextAddr, kon) = step (e1, env, store, nextAddr, kon)

-- Getting the address for an addressed variable.
step (AddressExpr (Var s), env, store, nextAddr, kon) = step (Value $ VRef $ lookupAddr s env, env, store, nextAddr, kon)
step (AddressExpr (GlobalVar s), env, store, nextAddr, kon) = step (Value $ VRef $ lookupAddr s globalEnv, env, store, nextAddr, kon)
    where (GlobalEnv globalEnv) = fromJust $ MapL.lookup storedGlobalEnv store

step (AddressExpr e1, env, store, nextAddr, kon) = step (e1, env, store, nextAddr, AddressExprFrame:kon)
step (Value e1, env, store, nextAddr, AddressExprFrame:kon) = step (Value $ VRef $ nextAddr, env, store', nextAddr+1, kon)
    where store' = updateStore store nextAddr e1


-- Function blocks ({ Expr }), which must have a 'return' statement.
step (FuncBlock e1, env, store, nextAddr, kon) = step (e1, env, store, nextAddr, FuncBlockFrame:kon)
step (Return e1, env, store, nextAddr, kon) = step (e1, env, store, nextAddr, ReturnFrame:kon)
step (Value e1, env, store, nextAddr, FuncBlockFrame:kon) = return (Value VNone, env, store, nextAddr, kon)

-- Returning from a function.
step (Value e1, env, store, nextAddr, ReturnFrame:kon) = return (Value e1, env, store, nextAddr, kon)

-- User-defined function calls.
step (FuncCall s ps, env, store, nextAddr, kon) = do
    (args, store') <- evaluateArgs ps env store nextAddr []
    (e1, env', store'', nextAddr') <- handleFuncArgs args env store' nextAddr s -- Pattern match
    (Value e2, _, store''', _, _) <- step (e1, env', store'', nextAddr', ReturnFrame:kon) -- Recurse into function call.

    step (Value e2, env, store''', nextAddr, kon) -- Continue after function call returns.

-- Built-in functions.
-- List (pass-by-value) operations:
step (BuiltInFunc "tail" [Var xs], env, store, nextAddr, kon)
    | length ys > 0 = return (Value $ VList t $ tail ys, env, store, nextAddr, kon)
    | otherwise = error "In tail function call, length of the list is 0."
    where 
        (VList t ys) = lookupVar xs env store

step (BuiltInFunc "head" [Var xs], env, store, nextAddr, kon)
    | length ys > 0 = return (Value $ head ys, env, store, nextAddr, kon)
    | otherwise = error "In head function call, length of the list is 0."
    where 
        (VList t ys) = lookupVar xs env store

step (BuiltInFunc "drop" [Var n, Var xs], env, store, nextAddr, kon) 
    | n' < 0 = error "Cannot pass in a negative value to drop function."
    | otherwise = return (Value $ VList t $ drop n' ys, env, store, nextAddr, kon)
    where 
        (VList t ys) = lookupVar xs env store
        (VInt n') = lookupVar n env store

step (BuiltInFunc "take" [Var n, Var xs], env, store, nextAddr, kon) 
    | n' < 0 = error "Cannot pass in a negative value to take function."
    | otherwise = return (Value $ VList t $ take n' ys, env, store, nextAddr, kon)
    where 
        (VList t ys) = lookupVar xs env store
        (VInt n') = lookupVar n env store

step (BuiltInFunc "length" [Var xs], env, store, nextAddr, kon) = return (Value $ VInt $ length ys, env, store, nextAddr, kon)
    where 
        (VList t ys) = lookupVar xs env store

step (BuiltInFunc "get" [Var n, Var xs], env, store, nextAddr, kon)
    | n' < 0 = error "Cannot pass in a negative value to get function."
    | n' >= length ys = error "IndexOutOfBoundsException in get function."
    | otherwise = return (Value $ ys!!n', env, store, nextAddr, kon)
    where 
        (VList t ys) = lookupVar xs env store
        (VInt n') = lookupVar n env store


-- List/Stream reference (pass-by-reference) operations:
step (BuiltInFunc "pop" [Var p], env, store, nextAddr, kon) = case v of
        Just (VList t xs) -> case (length xs > 0) of
                True  -> return (Value $ head xs, env, updateStore store r (VList t (tail xs)), nextAddr, kon)
                False -> error $ "Length of referenced list must be at least 1 in pop function."

        Just (VStream i xs) -> do
                (ys, store') <- dropStreamInput (VStream i xs) 1 store
                case ys of
                        [VInt n] -> return (Value $ VInt n, env, store', nextAddr, kon)
                        _        -> error "EndOfStreamException - stream is empty in pop function."

    where 
        (VRef r) = lookupVar p env store
        v = MapL.lookup r store

step (BuiltInFunc "popN" [Var n, Var p], env, store, nextAddr, kon) = case v of
        Just (VList t xs) -> case (length xs >= n') of
                True  -> return (Value $ VList t $ take n' xs, env, updateStore store r (VList t (drop n' xs)), nextAddr, kon)
                False -> error $ "Length of referenced list must be greater than or equal to 'n' in popN function."

        Just (VStream i xs) -> do
                (ys, store') <- dropStreamInput (VStream i xs) n' store
                case (ys == [] || VNone `elem` ys) of
                        True  -> error "EndOfStreamException - unable to pop 'n' elements from the stream."
                        False ->  return (Value $ VList (TList TInt) ys, env, store', nextAddr, kon)
    
    where 
        (VInt n') = lookupVar n env store
        (VRef r) = lookupVar p env store
        v = MapL.lookup r store

step (BuiltInFunc "peek" [Var p], env, store, nextAddr, kon) = case v of
        Just (VList t xs) -> case (length xs > 0) of
                True  -> return (Value $ head xs, env, store, nextAddr, kon)
                False -> error $ "Length of referenced list must be at least 1 in peek function."

        Just (VStream i xs) -> do
                (ys, store') <- peekStreamInput (VStream i xs) 1 store
                case ys of
                        [VInt n] -> return (Value $ VInt n, env, store', nextAddr, kon)
                        _        -> error "EndOfStreamException - stream is empty in peek function."

    where 
        (VRef r) = lookupVar p env store
        v = MapL.lookup r store

step (BuiltInFunc "peekN" [Var n, Var p], env, store, nextAddr, kon) = case v of
        Just (VList t xs) -> case (length xs >= n') of
                True  -> return (Value $ VList t $ take n' xs, env, updateStore store r (VList t xs), nextAddr, kon)
                False -> error $ "Length of referenced list must be greater than or equal to 'n' in peekN function."

        Just (VStream i xs) -> do
                (ys, store') <- peekStreamInput (VStream i xs) n' store
                case (ys == [] || VNone `elem` ys) of
                        True  -> error "EndOfStreamException - unable to peek 'n' elements from the stream."
                        False ->  return (Value $ VList (TList TInt) ys, env, store', nextAddr, kon)
    
    where 
        (VInt n') = lookupVar n env store
        (VRef r) = lookupVar p env store
        v = MapL.lookup r store

step (BuiltInFunc "isEmpty" [Var p], env, store, nextAddr, kon) = case v of
        Just (VList t xs) -> return (Value $ VBool (length xs == 0), env, store, nextAddr, kon)

        Just (VStream i xs) -> do
                (ys, store') <- peekStreamInput (VStream i xs) 1 store
                return (Value $ VBool (length ys == 0 || VNone `elem` ys), env, store', nextAddr, kon)

    where 
        (VRef r) = lookupVar p env store
        v = MapL.lookup r store

step (BuiltInFunc "hasElems" [Var n, Var p], env, store, nextAddr, kon) = case v of
        Just (VList t xs) -> return (Value $ VBool (length xs >= n'), env, store, nextAddr, kon)

        Just (VStream i xs) -> do
                (ys, store') <- peekStreamInput (VStream i xs) n' store
                return (Value $ VBool (length ys >= n' && not (VNone `elem` ys)), env, store', nextAddr, kon)

    where 
        (VInt n') = lookupVar n env store
        (VRef r) = lookupVar p env store
        v = MapL.lookup r store


-- IO Operations:
step (BuiltInFunc "out" [Var a], env, store, nextAddr, kon) = do
    putStrLn $ show v
    return (Value VNone, env, store, nextAddr, kon)
        where 
            v = (lookupVar a env store)

step (BuiltInFunc "in" [Var n], env, store, nextAddr, kon) = return (Value $ VRef $ -n', env, store', nextAddr, kon)
    where 
        (VInt n') = (lookupVar n env store)
        store' = case MapL.lookup (-n') store of
                Just (VStream i xs) -> store
                Nothing -> updateStore store (-n') (VStream n' [])

step (BuiltInFunc "setIn" [Var xs], env, store, nextAddr, kon) = return (Value VNone, env, store, nextAddr, kon)
    where 
        (VList t ys) = lookupVar xs env store
        store' = MapL.insert (inputStreamsToRead) (VList t (sort ys)) $ 
                        foldr (\(VInt x) store' -> case MapL.lookup (-x) store' of
                                Just (VStream i ys) -> store'
                                Nothing -> MapL.insert (-x) (VStream x []) store'
                        ) store ys


-- Math binary operations.
step (Op (MathOp op e1 e2), env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (HBinOp $ BinMathOp op e2 env):kon)
step (Value e1, env, store, nextAddr, (HBinOp (BinMathOp op e2 env')):kon) = step (e2, env', store, nextAddr, (BinOpH $ BinMathOp op (Value e1) env):kon)
step (Value (VInt n'), env', store, nextAddr, (BinOpH (BinMathOp op (Value (VInt n)) env)):kon) = step (Value $ VInt r, env, store, nextAddr, kon)
    where r = case op of
                    Plus -> n + n'
                    Min -> n - n'
                    Mult -> n * n'
                    Div -> n `div` n'
                    Exp -> n ^ n'
                    Mod -> n `mod` n'

-- Adding 2 lists together.
step (Value (VList t' n'), env', store, nextAddr, (BinOpH (BinMathOp Plus (Value (VList t n)) env)):kon)
    | newT /= TConflict = step (Value $ VList newT (n ++ n'), env, store, nextAddr, kon)
    | otherwise = error $ "Type error \n" ++ (show $ Value (VList t n)) ++ "\n" ++(show Plus)++ "\n" ++(show $ Value (VList t' n'))
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
step (Op (CompOp op e1 e2), env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (HBinOp $ BinCompOp op e2 env):kon)
step (Value e1, env, store, nextAddr, (HBinOp (BinCompOp op e2 env')):kon) = step (e2, env', store, nextAddr, (BinOpH $ BinCompOp op (Value e1) env):kon)

-- Boolean &&, || operations.
step (Value (VBool b'), env', store, nextAddr, (BinOpH (BinCompOp And (Value (VBool b)) env)):kon)
    | b' = step (Value $ VBool $ b && b', env, store, nextAddr, kon)
    | otherwise = step (Value $ VBool False, env, store, nextAddr, kon)
step (Value e2, env', store, nextAddr, (BinOpH (BinCompOp And (Value e1) env)):kon) = error $ "Type error \n" ++ (show $ Value e1)++"\n"++(show And)++"\n"++(show $ Value e2) ++"\nBoolean"

step (Value (VBool b'), env', store, nextAddr, (BinOpH (BinCompOp Or (Value (VBool b)) env)):kon) 
    | b' = step (Value $ VBool True, env, store, nextAddr, kon)
    | otherwise = step (Value $ VBool $ b || b', env, store, nextAddr, kon)
step (Value e2, env', store, nextAddr, (BinOpH (BinCompOp Or (Value e1) env)):kon) = error $ "Type error \n" ++ (show $ Value e1)++"\n"++(show Or)++"\n"++(show $ Value e2) ++"\nBoolean"

-- Comparison <, >, == operations.
step (Value e1, env', store, nextAddr, (BinOpH (BinCompOp LessThan (Value e2) env)):kon) 
    | isChildOf t1 COrd && isChildOf t2 COrd && t1 == t2 = step (Value $ VBool $ e1 < e2, env, store, nextAddr, kon)
    | otherwise = error $ "Type error \n" ++ (show $ Value e1)++"\n"++(show LessThan)++"\n"++(show $ Value e2) ++"\nInteger"
    where
        t1 = getType store e1
        t2 = getType store e2

step (Value e1, env', store, nextAddr, (BinOpH (BinCompOp GreaterThan (Value e2) env)):kon) 
    | isChildOf t1 COrd && isChildOf t2 COrd && t1 == t2 = step (Value $ VBool $ e1 > e2, env, store, nextAddr, kon)
    | otherwise = error $ "Type error \n" ++ (show $ Value e1)++"\n"++(show GreaterThan)++"\n"++(show $ Value e2) ++"\nInteger"
    where
        t1 = getType store e1
        t2 = getType store e2

step (Value e2, env', store, nextAddr, (BinOpH (BinCompOp Equality (Value e1) env)):kon)
    | isChildOf t1 CEq && isChildOf t2 CEq && t1 == t2 = step (Value $ VBool $ e1 == e2, env, store, nextAddr, kon)
    | otherwise = error $ "Type error \n" ++ (show $ Value e1)++"\n"++(show Equality)++"\n"++(show $ Value e2)
    where
        t1 = getType store e1
        t2 = getType store e2


-- Cons operation.
step (Op (Cons e1 e2), env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (HBinOp $ BinConsOp e2 env):kon)
step (Value e1, env, store, nextAddr, (HBinOp (BinConsOp e2 env')):kon) = step (e2, env', store, nextAddr, (BinOpH (BinConsOp (Value e1) env')):kon)
step (Value (VList t xs), env, store, nextAddr, (BinOpH (BinConsOp (Value e1) env')):kon)
    | t' /= TConflict = step (Value (VList t' (e1:xs)), env', store, nextAddr, kon)
    | otherwise = error $ "Type conflict trying to cons " ++ (show e1) ++ " to VList " ++ (show xs)
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
        
        consTypes _ _ = TConflict


-- if-elif-else statement.
step (If c e1 e2, env, store, nextAddr, kon) = step (c, env, store, nextAddr, (HTerOp $ TerIfOp e1 e2):kon)
step (Value (VBool b), env, store, nextAddr, (HTerOp (TerIfOp e1 e2)):kon)
    | b = step (e1, env, store, nextAddr, kon)
    | otherwise = step $ helper e2
    where helper Nothing = (Value VNone, env, store, nextAddr, kon)
          helper (Just (Else e)) = (e, env, store, nextAddr, kon)
          helper (Just (Elif c' e1' e2')) = (If c' e1' e2', env, store, nextAddr, kon)


-- While loop.
step (While c e1, env, store, nextAddr, kon) = step (c, env, store, nextAddr, (HTerOp $ TerWhileOp c e1):kon)
step (Value (VBool b), env, store, nextAddr, (HTerOp (TerWhileOp c e1)):kon)
    | b = step (e1, env, store, nextAddr, (TerOpH $ TerWhileOp c e1):kon)
    | otherwise = step (Value VNone, env, store, nextAddr, kon)
step (Value v, env, store, nextAddr, (TerOpH (TerWhileOp c e1)):kon) = step (c, env, store, nextAddr, (HTerOp $ TerWhileOp c e1):kon)


-- For loop.
step (For i c n e, env, store, nextAddr, kon) = step (i, env, store, nextAddr, (HTerOp (TerForInit i c n e)):kon)
step (Value _, env, store, nextAddr, (HTerOp (TerForInit i c n e)):kon) = step (c, env, store, nextAddr, (HTerOp (TerForOp c n e)):kon) -- Initialised vars.
step (Value (VBool b), env, store, nextAddr, (HTerOp (TerForOp c n e)):kon) -- Evaluated condition.
    | b = step (e, env, store, nextAddr, (TerOp_H (TerForOp c n e)):kon)
    | otherwise = step (Value VNone, env, store, nextAddr, kon)
step (Value _, env, store, nextAddr, (TerOp_H (TerForOp c n e)):kon) = step (n, env, store, nextAddr, (TerOpH (TerForOp c n e)):kon) -- Evaluated expression
step (Value _, env, store, nextAddr, (TerOpH (TerForOp c n e)):kon) = step (c, env, store, nextAddr, (HTerOp (TerForOp c n e)):kon) -- Incremented vars.


-- End of evaluation.
step s@(_, _, _, _, [Done]) = return s

-- No defined step for the current State.
step s@(exp, env, store, nextAddr, kon) = error $ "ERROR evaluating expression " ++ (show s) ++ ", no CESK step defined."


-- Evaluate arguments to an ExprValue.
evaluateArgs :: Parameters -> Environment -> Store -> Address -> [ExprValue] -> IO ([ExprValue], Store)
evaluateArgs FuncParamEnd env store nextAddr ls = return (ls, store)
evaluateArgs (FuncParam e1 e2) env store nextAddr ls = do
    (Value e1',_,store',_,_) <- step (e1, env, store, nextAddr, [Done])
    evaluateArgs e2 env store' nextAddr (ls ++ [e1'])


-- Pattern matches a function parameters with some given arguments, returning the functions Expr value, as well as updated Env, Store, and next Address.
-- Takes in the unevaluated arguments, the current Env, Store and next Address, as well as the function name.
handleFuncArgs :: [ExprValue] -> Environment -> Store -> Address -> String -> IO (Expr, Environment, Store, Address)
handleFuncArgs args env store nextAddr s = do

    let fv@(VFunc (TFunc ts _ cs) _) = (lookupVar s env store)

    case validType store cs ts args of
        False -> error "Type error, invalid argument for function."

        True -> do
            (e1, xs, store') <- matchArgsToFunc store args fv
            let (store'') = let (Just (GlobalEnv e)) = MapL.lookup storedGlobalEnv store' in if (e == Map.empty) 
                    then (MapL.update (\x -> Just $ GlobalEnv env) storedGlobalEnv store') 
                    else (store') -- update Global Env
            let (env', store''', nextAddr') = 
                    foldr (\(s, e2) (accEnv, accStore, addr) -> (overrideEnvStore accEnv accStore addr s e2)) (Map.empty, store'', nextAddr) xs

            return (e1, env', store''', nextAddr')

-- Takes in a list of evaluated arguments, and a list of evaluated function parameters.
-- Returns the function Expr value to use, as well as a list of Strings to ExprValues which need to be added to the Env and Store.
matchArgsToFunc :: Store -> [ExprValue] -> ExprValue -> IO (Expr, [(String, ExprValue)], Store)
matchArgsToFunc _ _ (VFunc t []) = error "No matching patterns for that function."
matchArgsToFunc store args (VFunc t ((ps,e1):xs)) = do
    (ys,store') <- foldr (\(p, a) acc -> do
                                (ls, store') <- acc

                                case ls of
                                    Nothing -> return (Nothing, store')

                                    Just (ls') -> (matchParamToArg p a ls' store')

                            ) (return (Just [], store)) (zip ps args)

    if (length args /= length ps  || ys == Nothing) 
        then matchArgsToFunc store' args (VFunc t xs) 
        else return (e1, fromJust ys, store')

        where

            matchParamToArg :: ExprValue -> ExprValue -> [(String, ExprValue)] -> Store -> IO (Maybe [(String, ExprValue)], Store)
            matchParamToArg (VList _ []) (VList _ []) ls store = return (Just ls, store)
            matchParamToArg (VList _ [VList _ []]) (VList _ []) ls store = return (Just ls, store)
            matchParamToArg (VList _ [VVar s]) (VList t ys) ls store = matchParamToArg (VVar s) (VList t ys) ls store

            matchParamToArg (VList t (x:xs)) (VList t' (y:ys)) ls store = do
                (e, store') <- matchParamToArg x y ls store
                case e of
                    Nothing -> return (Nothing, store')
                    Just (ls') -> matchParamToArg (VList t xs) (VList t' ys) ls' store'

            matchParamToArg l@(VPointerList t xs) r@(VRef a) ls store
                | v /= Nothing && ((isTList $ getType store v') || getType store v' == TStream) = matchPointerListToRef l r v' ls store 0
                | otherwise = return (Nothing, store)
                where v = MapL.lookup a store
                      (Just v') = v

                      isTList (TList _) = True
                      isTList _ = False

            matchParamToArg (VPointer s) e2@(VRef a) ls store = return (Just ((s, e2):ls), store)
            matchParamToArg _ e2@(VRef a) ls store = return (Nothing, store)
            matchParamToArg (VVar s) e2 ls store = return (Just ((s, e2):ls), store)
            matchParamToArg (VInt n) (VInt n') ls store = return $ if n == n' then (Just ls, store) else (Nothing, store)
            matchParamToArg (VBool b) (VBool b') ls store = return $ if b == b' then (Just ls, store) else (Nothing, store)
            matchParamToArg _ _ _ store = return (Nothing, store)

            matchPointerListToRef :: ExprValue -> ExprValue -> ExprValue -> [(String, ExprValue)] -> Store -> Int -> IO (Maybe [(String, ExprValue)], Store)
            matchPointerListToRef (VPointerList t []) _ _ ls store c = return (Just ls, store)
            matchPointerListToRef (VPointerList _ [VList _ []]) r (VList _ []) ls store c = return (Just ls, store)
            matchPointerListToRef (VPointerList _ [VList _ []]) r st@(VStream _ _) ls store c = do
                (xs, store') <- peekStreamInput st (c+1) store
                if ((length xs)-1 < c || xs!!c /= VNone)
                            then return (Nothing, store')
                            else do
                                (_, store'') <- dropStreamInput st c store'
                                return (Just ls, store'')

            matchPointerListToRef (VPointerList _ [VVar s]) r@(VRef a) e@(VList t ys) ls store c = 
                return (Just ((s,r):ls), MapL.update (\x -> Just $ e) a store)

            matchPointerListToRef (VPointerList _ [VVar s]) r@(VRef a) st@(VStream _ _) ls store c = do
                (_, store') <- dropStreamInput st c store
                return (Just ((s,r):ls), store')

            matchPointerListToRef (VPointerList t (x:xs)) r (VList t' (y:ys)) ls store c = do
                (e, store') <- matchParamToArg x y ls store
                case e of
                    Nothing -> return (Nothing, store')
                    Just (ls') -> matchPointerListToRef (VPointerList t xs) r (VList t' ys) ls' store' c

            matchPointerListToRef (VPointerList t (x:xs)) r st@(VStream i _) ls store c = do
                (ys, store') <- peekStreamInput st (c+1) store

                if ((length ys)-1 < c) 
                    then return (Nothing, store')
                    else case ys!!c of
                            VNone -> return (Nothing, store')
                            
                            y -> do
                                (e, store'') <- matchParamToArg x y ls store'
                                case e of
                                    
                                    Nothing -> return (Nothing, store'')
                                    
                                    Just (ls') -> matchPointerListToRef (VPointerList t xs) r st ls' store'' (c+1)

                                        -- Evaluate and pair params to stream values.
                                        -- Then, only drop items from the stream if doesn't return Nothing.
                                        -- Maintain a counter c for the number of items to drop.
                                        -- Start with c = 0
                                        -- When peeking, peek c items, and get the c'th element
                                        -- If e /= Nothing, then recurse with c+1.

            matchPointerListToRef _ _ _ _ store _ = return (Nothing, store)

-- TODO:
-- * Use generics in function type checking
-- * Finish compareTypes
-- * Finish getType
-- * Check that a VList holds a type
-- * Enforce that VLists can only have one type (i.e. Haskell-style lists)
-- * Whenever you first build a VList, ensure you are setting its type correctly
-- * Check operations for type checking
-- * Provide types for all built-in functions.
-- * Itr should have a type a -> e.g. in params put:
--      type f (Itr a) -> a

-- Get the type of a value.
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

evaluateListType :: Store -> [ExprValue] -> Type -> Type -- Used for checking list types of parameter lists
evaluateListType store (x:xs) TParamList = evaluateListType store xs (getType store x)
evaluateListType store ((VInt _):xs) TInt = evaluateListType store xs TInt
evaluateListType store ((VBool _):xs) TBool = evaluateListType store xs TBool
evaluateListType store ((VPointer _):xs) t@(TRef r) = evaluateListType store xs t
evaluateListType store ((VVar _):xs) t = evaluateListType store xs t
evaluateListType store (VNone:xs) TNone = evaluateListType store xs TNone
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

-- Comparing value types.
validType :: Store -> [(String, TypeClass)] -> [Type] -> [ExprValue] -> Bool
validType store tc [] [] = True
validType store tc (_:ts) ((VPointer _):es) = validType store tc ts es
validType store tc (_:ts) ((VVar _):es) = validType store tc ts es
validType store tc (t:ts) (e:es) = compareTypes tc t (getType store e) && (validType store tc ts es)
validType _ _ _ _ = False

compareTypes :: [(String, TypeClass)] -> Type -> Type -> Bool
compareTypes tc (TFunc [] out ftc) (TFunc [] out' ftc') = compareTypes tc out out'
compareTypes tc (TFunc (p:ps) out ftc) (TFunc (p':ps') out' ftc') = compareTypes tc p p' && compareTypes tc (TFunc ps out ftc) (TFunc ps' out' ftc')
compareTypes tc (TList e1) (TList e2) = compareTypes tc e1 e2
compareTypes tc (TRef e1) (TRef e2) = compareTypes tc e1 e2
compareTypes tc (TGeneric s) e2
    | c == Nothing = True -- Generic can be anything
    | otherwise = isChildOf e2 (fromJust c)
    where c = lookup s tc
compareTypes tc (TIterable g) (TStream) = compareTypes tc g TInt
compareTypes tc (TIterable g) (TList e2) = compareTypes tc g e2
compareTypes tc e1 e2 = e1 == e2

-- Check if a type is a child of a type class.
isChildOf :: Type -> TypeClass -> Bool

-- Eq class.
isChildOf TInt CEq = True
isChildOf TBool CEq = True
isChildOf TEmpty CEq = True
isChildOf TNone CEq = True
isChildOf (TList e1) CEq = isChildOf e1 CEq
isChildOf _ CEq = False

-- Itr class.
isChildOf (TList _) CItr = True
isChildOf TStream CItr = True
isChildOf _ CItr = False

-- Ord class.
isChildOf TInt COrd = True
isChildOf (TList e1) COrd = isChildOf e1 COrd
isChildOf _ COrd = False


-- Lookup a variable in the Environment (if not in local env, then search global env) and Store. 
-- Throw an error if it can't be found, else return its corresponding ExprValue.
lookupVar :: String -> Environment -> Store -> ExprValue
lookupVar s env store
    | localAddr /= Nothing = case MapL.lookup (fromJust localAddr) store of -- local var exists.
                                    Nothing -> error $ "Value " ++ s ++ " is not in the Store -> " ++ (show store)
                                    Just (e1) -> e1
    | globalAddr /= Nothing = case MapL.lookup (fromJust globalAddr) store of -- local var doesn't exist, but global var does.
                                    Nothing -> error $ "Value " ++ s ++ " is not in the Store -> " ++ (show store)
                                    Just (e1) -> e1
    | otherwise =  error $ "Value " ++ s ++ " is not in the Environment (has not been defined) -> " ++ (show env)
    where localAddr = Map.lookup s env
          globalAddr = Map.lookup s globalEnv
          (GlobalEnv globalEnv) = fromJust $ MapL.lookup storedGlobalEnv store

lookupPointerVar :: String -> Environment -> Store -> ExprValue
lookupPointerVar s env store
    | (isTRef $ getType store val) = fromJust $ MapL.lookup r store
    | otherwise = error "Error, variable is not a pointer!"
    where val = lookupVar s env store
          (VRef r) = val

          isTRef (TRef _) = True
          isTRef _ = False

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
    where lookupInEnv = Map.lookup s env
          (env', addr) = case lookupInEnv of
                                Just (a) -> (env, a)
                                Nothing -> addToEnv env store nextAddr s

-- Adds a new String to the Environment, and returns a tuple of the new Environment and the created Address.
-- Assumes the String is not in the Environment.
addToEnv :: Environment -> Store -> Address -> String -> (Environment, Address)
addToEnv env store nextAddr s = (Map.insert s addr env, addr)
    where addr = case Map.lookup s env of 
                        Just (a) -> a
                        Nothing -> nextAddr

-- Updates an Address mapping in the store.
-- If the inputted Address is not in the store, then it will be inserted.
updateStore :: Store -> Address -> ExprValue -> Store

updateStore store a e@(VFunc ft [])
    | v == Nothing = MapL.insert a e store
    | otherwise = error "Error, functions type has already been declared!" -- Change me to allow functions to be overriden by local functions of the same name!
    where v = MapL.lookup a store

updateStore store a (VFuncUnTypedDef xs)
    | v == Nothing = error "Function type must be declared before the function definition!"
    | otherwise = let (Just (VFunc (TFunc ts out cs) ys)) = v in 
        case (length xs' == length ts && validType store cs ts xs') of
                True -> MapL.update (\x -> Just (VFunc (TFunc ts out cs) (ys ++ xs))) a store
                False -> error $ "Function definition doesn't match the function type.\n" ++ "Function def is " ++ (show xs) ++ "\nFunction type is " ++ (show (TFunc ts out cs))
    where v = MapL.lookup a store
          xs' = (fst $ head xs)

updateStore store a e1
    | item == Nothing = MapL.insert a e1 store
    | otherwise = MapL.update (\x -> Just e1) a store
    where item = MapL.lookup a store

-- Takes in stream num, num of values to get from the buffer, and the current store.
-- Returns updated store and list of values retrieved.
peekStreamInput :: ExprValue -> Int -> Store -> IO ([ExprValue], Store)
peekStreamInput st@(VStream i _) n store = do
    (VStream i' ys, store') <- handleStreamInput st n store
    return (take n ys, store')

peekStreamInput _ _ _ = error "peekStreamInput function only takes in a VStream type."

-- Takes in stream num, num of values to get from the buffer, and the current store.
-- Returns updated store and list of values retrieved.
dropStreamInput :: ExprValue -> Int -> Store -> IO ([ExprValue], Store)
dropStreamInput st@(VStream i _) n store = do
    (VStream i' ys, store') <- handleStreamInput st n store
    return (take n ys, MapL.update (\x -> Just (VStream i' $ drop n ys)) (-i') store')

dropStreamInput _ _ _ = error "dropStreamInput function only takes in a VStream type."

handleStreamInput :: ExprValue -> Int -> Store -> IO (ExprValue, Store) -- Update to just take in address!
handleStreamInput (VStream i _) n store = do
    let (VStream _ xs) = fromJust $ MapL.lookup (-i) store
    store' <- case length xs >= n of
                        True -> return store
                        False -> do updateStreams (n - length xs) store

    return (fromJust $ MapL.lookup (-i) store', store')

-- Input function. Reads in n values from a given sequence.
updateStreams :: Int -> Store -> IO Store
updateStreams n store = do
    xss <- readInput [] n

    let ks = case MapL.lookup (inputStreamsToRead) store of
                    Just (VList t ks') -> foldr (\(VInt k) acc -> k:acc) [] ks'
                    Nothing -> []

    return $ case xss of
                [] -> case ks of
                        [] -> helper 0 store
                        _ -> foldl (\store' k -> MapL.update (\(VStream i ys) -> Just (VStream i (ys++[VNone]))) (-k) store') store ks
                _ -> case ks of 
                        [] -> fst $ foldl (\(store', c) xs -> case MapL.lookup c store' of
                                                    Just (VStream i ys) -> (MapL.update (\x -> Just (VStream i (ys++xs))) c store', c-1)
                                                    Nothing -> (MapL.insert c (VStream (abs c) xs) store', c-1)
                                                    _ -> error "Should be a VStream in negative Store addresses."
                                    ) (store, 0) xss
                        _ -> case (length xss)-1 < (last ks) of
                                    False -> foldl (\store' k -> MapL.update (\(VStream i ys) -> Just (VStream i (ys++(xss!!k)))) (-k) store') store ks
                                    True -> error "Input length is less than the specified number of input streams."

        where helper c store = case MapL.lookup c store of
                                    Just (VStream i ys) -> helper (c-1) (MapL.update (\x -> Just (VStream i (ys++[VNone]))) c store)
                                    Nothing -> store

-- Read n lines of input into stream buffers (a list of lists).
readInput :: [[ExprValue]] -> Int -> IO [[ExprValue]]
readInput xss 0 = return xss
readInput [] n = do
    end <- isEOF

    if end
        then return []
        else do line <- getLine
                let line' = foldr (\x acc -> [VInt x]:acc) [] $ map (read :: String -> Int) $ words line
                readInput line' (n-1)

readInput xss n = do 
    end <- isEOF

    if end 
        then return (helper xss (replicate (length xss) VNone))
        else do line <- getLine
                let line' = map ((\i -> VInt i) . (read :: String -> Int)) $ words line
                let xss' = if (length xss /= length line') 
                                then (error "Error in readInput function, input is not in the correct format.") 
                                else (helper xss line')
                readInput xss' (n-1)

        where helper [] [] = []
              helper (ys:yss) (x:xs) = (ys ++ [x]) : helper yss xs

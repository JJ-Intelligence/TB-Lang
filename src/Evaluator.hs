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
storedGlobalEnv = 12 -- Address of the function CallStack.
inputStreamsToRead = 13
heapStart = 20 -- Starting address of the variable/function heap (space after the reserved area).

-- garbageSize = 10 -- Number of out-of-scope variables allowed in the heap before garbage collection kicks in. (NOW REDUNDANT)

-- Insert reserved items into the Environment and Store.
insertReserved :: Environment -> Store -> (Environment, Store)
insertReserved env store = helper ls env (MapL.insert storedGlobalEnv (GlobalEnv Map.empty) store)
    where ls = [("tail", 1, (VFunc (TFunc [TList $ TGeneric "a"] (TList $ TGeneric "a") []) [([VVar "xs"], BuiltInFunc "tail" [Var "xs"])])),
                ("head", 2, (VFunc (TFunc [TList $ TGeneric "a"] (TGeneric "a") []) [([VVar "xs"], BuiltInFunc "head" [Var "xs"])])),
                ("drop", 3, (VFunc (TFunc [TInt, TGeneric "a"] (TGeneric "a") [("a", CItr)]) [([VVar "n", VVar "xs"], BuiltInFunc "drop" [Var "n", Var "xs"])])),
                ("take", 4, (VFunc (TFunc [TInt, TGeneric "a"] (TGeneric "a") [("a", CItr)]) [([VVar "n", VVar "xs"], BuiltInFunc "take" [Var "n", Var "xs"])])),
                ("length", 5, (VFunc (TFunc [TList $ TGeneric "a"] (TInt) []) [([VVar "xs"], BuiltInFunc "length" [Var "xs"])])),
                ("get", 6, (VFunc (TFunc [TInt, TList $ TGeneric "a"] (TGeneric "a") []) [([VVar "n", VVar "xs"], BuiltInFunc "get" [Var "n", Var "xs"])])),
                ("out", 7, (VFunc (TFunc [TGeneric "a"] (TNone) []) [([VVar "v"], BuiltInFunc "out" [Var "v"])])),
                ("in", 8, (VFunc (TFunc [TInt] (TRef $ TStream) []) [([VVar "v"], BuiltInFunc "in" [Var "v"])])),
                ("setIn", 9, (VFunc (TFunc [TList $ TInt] (TNone) []) [([VVar "xs"], BuiltInFunc "setIn" [Var "xs"])])),
                ("pop", 10, (VFunc (TFunc [TGeneric "a"] (TGeneric "a") [("a", CItr)]) [([VVar "xs"], BuiltInFunc "pop" [Var "xs"])])),
                ("hasElems", 11, (VFunc [([VVar "n", VVar "xs"], BuiltInFunc "hasElems" [Var "n", Var "xs"])]))]
          helper xs env store = foldr (\(s,a,e) (env', store') -> (Map.insert s a env', MapL.insert a e store')) (env, store) xs

-- interpret :: String -> State
-- interpret s = eval $ parse $ alexScanTokens s

-- compiler :: Expr -> State
-- compiler e = (e, env, store, heapStart, [Done])

-- Start the evaluator by passing it an Expression (from the Parser).
startEvaluator :: Expr -> IO ()
startEvaluator e = do
    s <- step (e, env, store, heapStart, [Done])
    putStrLn "\nFinished evaluation.\n"
        where 
            (env, store) = insertReserved (Map.empty) (MapL.empty)

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
        evaluateParams (FuncParam (ExprType t) e3) = t : evaluateParams

        evaluateOut (ExprType t) = t
        evaluateOut (FuncType ps' out' cs') = buildTFunc ps' out' cs'

        evaluateConstraints FuncParamEnd = []
        evaluateConstraints (FuncParam (TypeConstraint cl g) e3) = (g, cl) : evaluateParams e3

        buildTFunc ps out cs = TFunc (evaluateParams ps) (evaluateOut out) (if cs == Nothing then [] else fromJust $ evaluateConstraints)

-- Defining a new Function.
step (LocalAssign (DefVar s (Func ps e1)), env, store, nextAddr, kon)
    | checkNoDuplicates [] $ usedVars params = step (Value fv, env', store', nextAddr', kon)
    | otherwise = error $ "Function '" ++ s ++"' has parameters containing multiple variables of the same name. \nIn " ++ (show $ Func ps e1)
    where 
        fv = lookupVar s env' store'
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
                helper (VList (x:xs)) = (helper x) ++ (helper (VList xs))
                helper (VPointerList (x:xs)) = (helper x) ++ (helper (VPointerList xs))
                helper _ = []

        evaluateParams FuncParamEnd = []
        evaluateParams (FuncParam (PointerExpr (Op (Cons e1 e2))) e3) = VPointerList (evalConsParam $ Op (Cons e1 e2)) : evaluateParams e3
        evaluateParams (FuncParam (Op (Cons e1 e2)) e3) = VList (evalConsParam $ Op (Cons e1 e2)) : evaluateParams e3
        evaluateParams (FuncParam e1 e2) = evalSingleParam e1 : evaluateParams e2

        evalSingleParam (Literal (EInt n)) = (VInt n)
        evalSingleParam (Literal (EBool b)) = (VBool b)
        evalSingleParam (Literal Empty) = (VList [])
        evalSingleParam (Var s) = (VVar s)
        evalSingleParam (PointerExpr (Var s)) = (VPointer s)
        evalSingleParam (PointerExpr e1) = VPointerList [evalSingleParam e1]

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
    | getType val == TRef = step (Value e1, env', store', nextAddr, kon)
    | otherwise = error "Pointer is not a reference!"
    where val = lookupVar s env store
          (VRef r) = val
          store' = updateStore store r e1

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
step (Return e1, env, store, nextAddr, FuncBlockFrame:kon) = step (e1, env, store, nextAddr, kon)
step (Value e1, env, store, nextAddr, FuncBlockFrame:kon) = step (Value VNone, env, store, nextAddr, kon)

-- User-defined function calls.
step (FuncCall s ps, env, store, nextAddr, kon) = do
    (args, store') <- evaluateArgs ps env store nextAddr []
    (e1, env', store'', nextAddr') <- handleFuncArgs args env store' nextAddr s -- Pattern match
    (Value e2, _, store''', _, _) <- step (e1, env', store'', nextAddr', ReturnFrame:kon) -- Recurse into function call.

    step (Value e2, env, store''', nextAddr, kon) -- Continue after function call returns.

-- Built-in functions.
step (BuiltInFunc "in" [Var s], env, store, nextAddr, kon)
    | getType v == TInt = step (Value $ VRef $ -n, env, store', nextAddr, kon)
    | otherwise = error "in function takes one parameter, an Int."
    where v = (lookupVar s env store)
          (VInt n) = v
          stream = MapL.lookup (-n) store
          store' = case stream of
                        Just (VStream i xs) -> store
                        Nothing -> updateStore store (-n) (VStream n [])

step (BuiltInFunc "out" [Var s], env, store, nextAddr, kon) = do
    putStrLn $ show v
    return (Value v, env, store, nextAddr, kon)
        where v = (lookupVar s env store)

step (BuiltInFunc "length" [Var xs], env, store, nextAddr, kon)
    | getType v == TList = let (VList xs) = v in step (Value $ VInt $ length xs, env, store, nextAddr, kon)
    | otherwise = error "Length function must take a list as an argument."
    where v = (lookupVar xs env store)

step (BuiltInFunc "head" [Var xs], env, store, nextAddr, kon)
    | getType v == TList = let (VList xs) = v in 
            case length xs of
                0 -> error "In head function call, length of the list is 0."
                _ -> step (Value $ head xs, env, store, nextAddr, kon)
    | otherwise = error "Head function must take a list as an argument."
    where v = (lookupVar xs env store)

step (BuiltInFunc "tail" [Var xs], env, store, nextAddr, kon)
    | getType v == TList = let (VList xs) = v in 
            case length xs of
                0 -> error "In head function call, length of the list is 0."
                _ -> step (Value $ VList (tail xs), env, store, nextAddr, kon)
    | otherwise = error "Tail function must take a list as an argument."
    where v = (lookupVar xs env store)

step s@(BuiltInFunc "drop" [Var num, Var list], env, store, nextAddr, kon) = handleTakeDrop (drop) s
step s@(BuiltInFunc "take" [Var num, Var list], env, store, nextAddr, kon) = handleTakeDrop (take) s


step (BuiltInFunc "get" [Var num, Var list], env, store, nextAddr, kon)
    | getType n == TInt && getType xs == TList = let (VList xs') = xs; (VInt n') = n in
            if n' < 0 
                then error "Get function can't take in a negative." 
                else case n' >= (length xs') of
                        True -> error $ "Get function index is larger than the length of the list."
                        False -> step (Value $ xs'!!n', env, store, nextAddr, kon)
    | otherwise = error "Get function takes in a list and an int"
    where n = (lookupVar num env store)
          xs = (lookupVar list env store)

step (BuiltInFunc "pop" [Var list], env, store, nextAddr, kon)
    | getType v == TRef && ls /= Nothing = 
        case (getType (fromJust ls)) of

                TList -> let (VList xs) = fromJust ls in 
                    case (length xs > 0) of
                        True -> step (Value $ head xs, env, updateStore store r (VList (tail xs)), nextAddr, kon)
                        False -> error $ "Length of list must be at least 1."

                TStream -> do
                    (ys, store') <- dropStreamInput (fromJust ls) 1 store
                    step (Value $ head ys, env, store', nextAddr, kon)

                _ -> error $ "Pop function takes in a reference to a list or stream."

    | otherwise = error $ "Pop function takes in a reference to a value."
    where v = lookupVar list env store
          (VRef r) = v
          ls = MapL.lookup r store

step (BuiltInFunc "hasElems" [Var num, Var stream], env, store, nextAddr, kon)
    | getType n == TInt && n' >= 0 && getType v == TRef && st /= Nothing && getType (fromJust st) == TStream = do
        (ys, store') <- peakStreamInput (fromJust st) n' store
        case (length ys < n') of
            True -> step (Value $ VBool False, env, store', nextAddr, kon)
            False -> let b = foldl (\acc x -> if (getType x == TNone) then False else acc) True ys in
                        step (Value $ VBool b, env, store', nextAddr, kon)
    | otherwise = error "Error in hasElems function."
    where n = (lookupVar num env store)
          (VInt n') = n
          v = lookupVar stream env store
          (VRef r) = v
          st = MapL.lookup r store

          helper [] = True
          helper ((VInt _):ys) = helper ys
          helper _ = False

step (BuiltInFunc "hasElems" [Var stream], env, store, nextAddr, kon) = step (BuiltInFunc "hasElems" [Var "n", Var stream], env', store', nextAddr', kon)
    where (env', store', nextAddr') = overrideEnvStore env store nextAddr "n" (VInt 1)

step (BuiltInFunc "setIn" [Var list], env, store, nextAddr, kon)
    | getType v == TList && length xs > 0 && getListType xs == TInt = step (Value VNone, env, store'', nextAddr, kon)
    where v = lookupVar list env store
          (VList xs) = v
          store' = foldr (\(VInt x) store' -> case MapL.lookup (-x) store' of
                                            Just (VStream i ys) -> store'
                                            Nothing -> MapL.insert (-x) (VStream x []) store'
                            ) store xs

          store'' = let ls = MapL.lookup (inputStreamsToRead) store' in
                        case ls of
                            Just (VList ys) -> MapL.update (\x -> Just (VList (sort xs))) (inputStreamsToRead) store'
                            Nothing -> MapL.insert (inputStreamsToRead) (VList (sort xs)) store'

          getListType [] = TInt
          getListType ((VInt _):xs) = getListType xs
          getListType _ = TConflict


-- Returning from a function.
step (Value e1, env, store, nextAddr, ReturnFrame:kon) = return (Value e1, env, store, nextAddr, kon)


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

step (Value (VList n'), env', store, nextAddr, (BinOpH (BinMathOp Plus (Value (VList n)) env)):kon)
    | getType (VList (n ++ n')) /= TConflict = step (Value $ VList (n ++ n'), env, store, nextAddr, kon)
    | otherwise = typeError (Value (VList n)) (show Plus) (Value (VList n')) []
step (Value e2, env', store, nextAddr, (BinOpH (BinMathOp op (Value e1) env)):kon) = typeError (Value e1) (show op) (Value e2) "Integer"


-- Binary comparison operations.
step (Op (CompOp op e1 e2), env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (HBinOp $ BinCompOp op e2 env):kon)
step (Value e1, env, store, nextAddr, (HBinOp (BinCompOp op e2 env')):kon) = step (e2, env', store, nextAddr, (BinOpH $ BinCompOp op (Value e1) env):kon)

-- Boolean &&, || operations.
step (Value (VBool b'), env', store, nextAddr, (BinOpH (BinCompOp And (Value (VBool b)) env)):kon)
    | b' = step (Value $ VBool $ b && b', env, store, nextAddr, kon)
    | otherwise = step (Value $ VBool False, env, store, nextAddr, kon)
step (Value e2, env', store, nextAddr, (BinOpH (BinCompOp And (Value e1) env)):kon) = typeError (Value e1) (show And) (Value e2) "Boolean"

step (Value (VBool b'), env', store, nextAddr, (BinOpH (BinCompOp Or (Value (VBool b)) env)):kon) 
    | b' = step (Value $ VBool True, env, store, nextAddr, kon)
    | otherwise = step (Value $ VBool $ b || b', env, store, nextAddr, kon)
step (Value e2, env', store, nextAddr, (BinOpH (BinCompOp Or (Value e1) env)):kon) = typeError (Value e1) (show Or) (Value e2) "Boolean"

-- Comparison <, >, == operations.
step (Value e1, env', store, nextAddr, (BinOpH (BinCompOp LessThan (Value e2) env)):kon) 
    | isChildOf t1 COrd && isChildOf t2 COrd && t1 == t2 = step (Value $ VBool $ e1 < e2, env, store, nextAddr, kon)
    | otherwise = typeError (Value e1) (show LessThan) (Value e2) "Integer"
    where
        t1 = getType e1
        t2 = getType e2

step (Value e1, env', store, nextAddr, (BinOpH (BinCompOp GreaterThan (Value e2) env)):kon) 
    | isChildOf t1 COrd && isChildOf t2 COrd && t1 == t2 = step (Value $ VBool $ e1 > e2, env, store, nextAddr, kon)
    | otherwise = typeError (Value e1) (show LessThan) (Value e2) "Integer"
    where
        t1 = getType e1
        t2 = getType e2

step (Value e2, env', store, nextAddr, (BinOpH (BinCompOp Equality (Value e1) env)):kon)
    | isChildOf t1 CEq && isChildOf t2 CEq && t1 == t2 = step (Value $ VBool $ e1 == e2, env, store, nextAddr, kon)
    | otherwise = typeError (Value e1) (show Equality) (Value e2) []
    where
        t1 = getType e1
        t2 = getType e2


-- Cons operation.
step (Op (Cons e1 e2), env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (HBinOp $ BinConsOp e2 env):kon)
step (Value e1, env, store, nextAddr, (HBinOp (BinConsOp e2 env')):kon) = step (e2, env', store, nextAddr, (BinOpH (BinConsOp (Value e1) env')):kon)
step (Value (VList t xs), env, store, nextAddr, (BinOpH (BinConsOp (Value e1) env')):kon)
    | consTypes (getType e1) t /= TConflict = step (Value (VList t' (e1:xs)), env', store, nextAddr, kon)
    | otherwise = error "Type conflict trying to cons " ++ (show e1) ++ " to VList " ++ (show xs)
    where 
        t' = consTypes (getType e1) t

        consTypes e1 (TList e2) 
            | e1 == e2 = TList e1
            | isEmptyList e2 = buildList
            where
                buildList e1 Empty = e1
                buildList e1 (TList e2) = TList (buildList e1 e2)

                isEmptyList Empty = True
                isEmptyList (TList e1) = isEmptyList e1
        
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


-- Take and Drop functions (as they're exactly the same apart from the Haskell function).
handleTakeDrop :: (Int -> [ExprValue] -> [ExprValue]) -> State -> IO State
handleTakeDrop func (BuiltInFunc name [Var num, Var list], env, store, nextAddr, kon)
    | getType n == TInt = case n' < 0 of
            True -> error "Drop/tail functions must take a positive int."
            False -> case getType xs of

                        TList -> let (VList xs') = xs in
                                case (length xs' >= n') of
                                    True -> step (Value $ VList (func n' xs'), env, store, nextAddr, kon)
                                    False -> error "Plz make list bigger."

                        TRef -> case (MapL.lookup r store) of 
                                Just (VList ys) -> case (length ys >= n') of 
                                        True -> let store' = updateStore store r (VList (func n' ys)) in 
                                                    step (Value $ VList (func n' ys), env, store', nextAddr, kon)
                                        False -> error "Plz make list bigger."

                                Just (VStream i ys) -> do
                                    (zs, store') <- dropStreamInput (VStream i ys) n' store
                                    step (Value $ VList zs, env, store', nextAddr, kon)

                                _ -> error $ "Drop/tail functions only takes a reference to a list or stream."

    | otherwise = error $ "Drop/tail functions takes in an int and a list. "
    where n = (lookupVar num env store)
          (VInt n') = n
          xs = (lookupVar list env store)
          (VRef r) = xs

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

    case validType cs ts args of
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
            matchParamToArg (VList []) (VList []) ls store = return (Just ls, store)
            matchParamToArg (VList [VList []]) (VList []) ls store = return (Just ls, store)
            matchParamToArg (VList [VVar s]) (VList ys) ls store = matchParamToArg (VVar s) (VList ys) ls store

            matchParamToArg (VList (x:xs)) (VList (y:ys)) ls store = do
                (e, store') <- matchParamToArg x y ls store
                case e of
                    Nothing -> return (Nothing, store')
                    Just (ls') -> matchParamToArg (VList xs) (VList ys) ls' store'

            matchParamToArg l@(VPointerList xs) r@(VRef a) ls store
                | v /= Nothing && (getType v' == TList || getType v' == TStream) = 
                    matchPointerListToRef l r v' ls store 0
                | otherwise = return (Nothing, store)
                where v = MapL.lookup a store
                      (Just v') = v

            matchParamToArg (VPointer s) e2@(VRef a) ls store = return (Just ((s, e2):ls), store)
            matchParamToArg _ e2@(VRef a) ls store = return (Nothing, store)
            matchParamToArg (VVar s) e2 ls store = return (Just ((s, e2):ls), store)
            matchParamToArg (VInt n) (VInt n') ls store = return $ if n == n' then (Just ls, store) else (Nothing, store)
            matchParamToArg (VBool b) (VBool b') ls store = return $ if b == b' then (Just ls, store) else (Nothing, store)
            matchParamToArg _ _ _ store = return (Nothing, store)

            matchPointerListToRef :: ExprValue -> ExprValue -> ExprValue -> [(String, ExprValue)] -> Store -> Int -> IO (Maybe [(String, ExprValue)], Store)
            matchPointerListToRef (VPointerList []) _ _ ls store c = return (Just ls, store)
            matchPointerListToRef (VPointerList [VList []]) r (VList []) ls store c = return (Just ls, store)
            matchPointerListToRef (VPointerList [VList []]) r st@(VStream _ _) ls store c = do
                (xs, store') <- peakStreamInput st (c+1) store
                if ((length xs)-1 < c || xs!!c /= VNone)
                            then return (Nothing, store')
                            else do
                                (_, store'') <- dropStreamInput st c store'
                                return (Just ls, store'')

            matchPointerListToRef (VPointerList [VVar s]) r@(VRef a) (VList ys) ls store c = 
                return (Just ((s,r):ls), MapL.update (\x -> Just $ VList ys) a store)

            matchPointerListToRef (VPointerList [VVar s]) r@(VRef a) st@(VStream _ _) ls store c = do
                (_, store') <- dropStreamInput st c store
                return (Just ((s,r):ls), store')

            matchPointerListToRef (VPointerList (x:xs)) r (VList (y:ys)) ls store c = do
                (e, store') <- matchParamToArg x y ls store
                case e of
                    Nothing -> return (Nothing, store')
                    Just (ls') -> matchPointerListToRef (VPointerList xs) r (VList ys) ls' store' c

            matchPointerListToRef (VPointerList (x:xs)) r st@(VStream i _) ls store c = do
                (ys, store') <- peakStreamInput st (c+1) store

                if ((length ys)-1 < c) 
                    then return (Nothing, store')
                    else case ys!!c of
                            VNone -> return (Nothing, store')
                            
                            y -> do
                                (e, store'') <- matchParamToArg x y ls store'
                                case e of
                                    
                                    Nothing -> return (Nothing, store'')
                                    
                                    Just (ls') -> matchPointerListToRef (VPointerList xs) r st ls' store'' (c+1)

                                        -- Evaluate and pair params to stream values.
                                        -- Then, only drop items from the stream if doesn't return Nothing.
                                        -- Maintain a counter c for the number of items to drop.
                                        -- Start with c = 0
                                        -- When peaking, peak c items, and get the c'th element
                                        -- If e /= Nothing, then recurse with c+1.

            matchPointerListToRef _ _ _ _ store _ = return (Nothing, store)

-- TODO:
-- * What is the type of a VPointerList - List (Ref TStream/TList) - VPointerList should store a type like VList
-- * Use generics in function type checking
-- * Finish compareTypes
-- * Finish getType
-- * Check that a VList holds a type
-- * Enforce that VLists can only have one type (i.e. Haskell-style lists)
-- * Whenever you first build a VList, ensure you are setting its type correctly
-- * Check operations for type checking
-- * Provide types for all built-in functions.
-- * Itr should have a type a -> e.g. in params put:

-- type f (Itr a) -> a

-- Get the type of a value.
getType :: ExprValue -> Type
getType (VInt _) = TInt
getType (VBool _) = TBool
getType (VStream _ _) = TStream
getType VNone = TNone
getType (VRef e1) = TRef (getType e1)
getType (VList t xs) = t
getType (VPointerList t xs) = TRef (TList TInt) -- Pointer lists only used for Stream pattern matching

-- Comparing value types.
validType :: [(String, TypeClass)] -> [Type] -> [ExprValue] -> Bool
validType tc [] [] = True
validType tc (_:ts) ((VPointer _):es) = validType ts es
validType tc (_:ts) ((VVar _):es) = validType ts es
validType tc (t:ts) (e:es) = compareTypes t (getType e) && (validType ts es)

compareTypes :: [(String, TypeClass)] -> Type -> ExprValue -> Bool
compareTypes tc (TFunc [] out) (TFunc [] out') = compareTypes out out'
compareTypes tc (TFunc (p:ps) out) (TFunc (p':ps') out') = compareTypes p p' && compareTypes (TFunc ps out) (TFunc ps' out')
compareTypes tc (TList e1) (TList e2) = compareTypes e1 e2
compareTypes tc (TRef e1) (TRef e2) = compareTypes e1 e2
compareTypes tc (TGeneric s) e2
    | c == Nothing = True -- Generic can be anything
    | otherwise = isChildOf e2 (fromJust c)
    where c = lookup s tc
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
isChildOf (TRef (TList _)) CItr = True
isChildOf (TRef (TStream)) CItr = True
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
    | getType val == TRef = fromJust $ MapL.lookup r store
    | otherwise = error "Error, variable is not a pointer!"
    where val = lookupVar s env store
          (VRef r) = val

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
        case (length ys == length ts && validType cs ts ys) of
                True -> MapL.update (\x -> Just (VFunc t (ys ++ xs))) a store
                Else -> error "Function definition doesn't match the function type."
    where v = MapL.lookup a store
          ys = (fst $ head xs)

updateStore store a e1
    | item == Nothing = MapL.insert a e1 store
    | otherwise = MapL.update (\x -> Just e1) a store
    where item = MapL.lookup a store

-- Takes in stream num, num of values to get from the buffer, and the current store.
-- Returns updated store and list of values retrieved.
peakStreamInput :: ExprValue -> Int -> Store -> IO ([ExprValue], Store)
peakStreamInput st@(VStream i _) n store = do
    (VStream i' ys, store') <- handleStreamInput st n store
    return (take n ys, store')

peakStreamInput _ _ _ = error "peakStreamInput function only takes in a VStream type."

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
                    Just (VList ks') -> foldr (\(VInt k) acc -> k:acc) [] ks'
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


-- Type error between Expr e1 and Expr e2, using operator String s, which uses type String t.
typeError :: Expr -> String -> Expr -> String -> a
typeError e1 s e2 [] = typeError e1 s e2 "the same"
typeError (Value e1) s (Value e2) t = error $ "\n\nType Error: '" ++ s ++ "' operator must be between " ++ t ++ " types, in " ++ (show e1) ++ " "++s++" " ++ (show e2) ++ 
                        ".\nThe type of expression 1 is " ++ (show $ getType e1) ++ ", but the type of expression 2 is " ++ (show $ getType e2) ++ ".\n"

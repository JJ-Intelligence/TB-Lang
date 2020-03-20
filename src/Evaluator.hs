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
    where ls = [("tail", 1, (VFunc [([VVar "xs"], BuiltInFunc "tail" [Var "xs"])])),
                ("head", 2, (VFunc [([VVar "xs"], BuiltInFunc "head" [Var "xs"])])),
                ("drop", 3, (VFunc [([VVar "n", VVar "xs"], BuiltInFunc "drop" [Var "n", Var "xs"])])),
                ("take", 4, (VFunc [([VVar "n", VVar "xs"], BuiltInFunc "take" [Var "n", Var "xs"])])),
                ("length", 5, (VFunc [([VVar "xs"], BuiltInFunc "length" [Var "xs"])])),
                ("get", 6, (VFunc [([VVar "n", VVar "xs"], BuiltInFunc "get" [Var "n", Var "xs"])])),
                ("out", 7, (VFunc [([VVar "v"], BuiltInFunc "out" [Var "v"])])),
                ("in", 8, (VFunc [([VVar "v"], BuiltInFunc "in" [Var "v"])])),
                ("setIn", 9, (VFunc [([VVar "xs"], BuiltInFunc "setIn" [Var "xs"])])),
                ("pop", 10, (VFunc [([VVar "xs"], BuiltInFunc "pop" [Var "xs"])])),
                ("hasElems", 11, (VFunc [([VVar "n", VVar "xs"], BuiltInFunc "hasElems" [Var "n", Var "xs"]),
                                            ([VVar "xs"], BuiltInFunc "hasElems" [Var "xs"])]))]
          helper xs env store = foldr (\(s,a,e) (env', store') -> (Map.insert s (a,Global) env', MapL.insert a e store')) (env, store) xs

-- interpret :: String -> State
-- interpret s = eval $ parse $ alexScanTokens s

-- Start the evaluator by passing it an Expression (from the Parser).
startEvaluator :: Expr -> IO ()
startEvaluator e = do
    s <- step (e, env, store, heapStart, [Done])
    putStrLn "\nFinished evaluation.\n"
        where (env, store) = insertReserved (Map.empty) (MapL.empty)

-- Step function to move from one State to another.
step :: State -> IO State

-- Converting Literals to Values.
step (Literal (EInt n), env, store, nextAddr, kon) = step (Value $ VInt n, env, store, nextAddr, kon)
step (Literal (EBool n), env, store, nextAddr, kon) = step (Value $ VBool n, env, store, nextAddr, kon)
step (Literal Empty, env, store, nextAddr, kon) = step (Value $ VList [], env, store, nextAddr, kon)
step (Literal ENone, env, store, nextAddr, kon) = step (Value VNone, env, store, nextAddr, kon)

-- Sequence operation ';'.
step (Seq e1 e2, env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (HBinOp $ BinSeqOp e2):kon)
step (Value e1, env, store, nextAddr, (HBinOp (BinSeqOp e2)):kon) = step (e2, env, store, nextAddr, kon)

-- Defining a new Function.
step (DefVar s (Func ps e1), env, store, nextAddr, kon) = step (Value funcVal, env', store', nextAddr', kon)
    where funcVal = VFunc [(evaluateParams ps, e1)]
          (env', store', nextAddr') = updateEnvStore env store nextAddr s funcVal

          evaluateParams FuncParamEnd = []
          evaluateParams (FuncParam (Op (Cons e1 e2)) e3) = VList (helper' $ Op (Cons e1 e2)) : evaluateParams e3
                where helper' (Op (Cons e1 e2)) = helper e1 : helper' e2
                      helper' e2 = [helper e2]

          evaluateParams (FuncParam e1 e2) = helper e1 : evaluateParams e2
          
          helper (Literal (EInt n)) = (VInt n)
          helper (Literal (EBool b)) = (VBool b)
          helper (Literal Empty) = (VList [])
          helper (Var s) = (VVar s)


-- Defining a new Var.
-- Looks for the variable in the Env, and replaces it in the Store if it exists, else it creates it.
step (DefVar s e1, env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (DefVarFrame s env):kon)
step (Value e1, env, store, nextAddr, (DefVarFrame s env'):kon) = step (Value e1, env'', store', nextAddr', kon)
    where (env'', store', nextAddr') = updateEnvStore env' store nextAddr s e1

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

-- Accessing a variable pointer.
step (PointerVar s, env, store, nextAddr, kon) = step (Value $ lookupPointerVar s env store, env, store, nextAddr, kon)

-- Getting the address for an addressed variable.
step (AddressExpr (Var s), env, store, nextAddr, kon) = step (Value $ VRef $ lookupAddr s env, env, store, nextAddr, kon)
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
    let (e1, env', store'', nextAddr') = handleFuncArgs args env store' nextAddr s -- Pattern match
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
        case length ys of
            0 -> step (Value $ VBool False, env, store', nextAddr, kon)
            _ -> case head ys of
                        (VInt _) -> step (Value $ VBool True, env, store', nextAddr, kon)
                        VNone -> step (Value $ VBool False, env, store', nextAddr, kon)
                        _ -> error "Something went wrong in the input."
    where n = (lookupVar num env store)
          (VInt n') = n
          v = lookupVar stream env store
          (VRef r) = v
          st = MapL.lookup r store

step (BuiltInFunc "hasElems" [Var stream], env, store, nextAddr, kon) = step (BuiltInFunc "hasElems" [Var "n", Var stream], env', store', nextAddr', kon)
    where (env', store', nextAddr') = overrideEnvStore env store nextAddr "n" (VInt 1) Local

step (BuiltInFunc "setIn" [Var list], env, store, nextAddr, kon)
    | getType v == TList && length xs > 0 && getListType xs == TInt = trace (show $ MapL.lookup (inputStreamsToRead) store'') $ step (Value VNone, env, store'', nextAddr, kon)
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

-- Binary comparison operations: ==, &&, ||, <, >
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

-- Comparison ==, <, > operations.
step (Value (VInt n'), env', store, nextAddr, (BinOpH (BinCompOp LessThan (Value (VInt n)) env)):kon) = step (Value $ VBool $ n < n', env, store, nextAddr, kon)
step (Value e2, env', store, nextAddr, (BinOpH (BinCompOp LessThan (Value e1) env)):kon) = typeError (Value e1) (show LessThan) (Value e2) "Integer"
step (Value (VInt n'), env', store, nextAddr, (BinOpH (BinCompOp GreaterThan (Value (VInt n)) env)):kon) = step (Value $ VBool $ n > n', env, store, nextAddr, kon)
step (Value e2, env', store, nextAddr, (BinOpH (BinCompOp GreaterThan (Value e1) env)):kon) = typeError (Value e1) (show GreaterThan) (Value e2) "Integer"

step (Value e2, env', store, nextAddr, (BinOpH (BinCompOp Equality (Value e1) env)):kon)
    | getType e1 == getType e2 = step (Value $ VBool $ e1 == e2, env, store, nextAddr, kon)
    | otherwise = typeError (Value e1) (show Equality) (Value e2) []

-- Cons binary operation.
step (Op (Cons e1 e2), env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (HBinOp $ BinConsOp e2 env):kon)
step (Value e1, env, store, nextAddr, (HBinOp (BinConsOp e2 env')):kon) = step (e2, env', store, nextAddr, (BinOpH (BinConsOp (Value e1) env')):kon)
step (Value (VList xs), env, store, nextAddr, (BinOpH (BinConsOp (Value e1) env')):kon) = step (Value (VList (e1:xs)), env', store, nextAddr, kon)

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
handleFuncArgs :: [ExprValue] -> Environment -> Store -> Address -> String -> (Expr, Environment, Store, Address)
handleFuncArgs args' env store nextAddr s = (e1, env', store'', nextAddr')
    where (env', store'', nextAddr') = foldr (\(s, e2) (accEnv, accStore, addr) -> (overrideEnvStore accEnv accStore addr s e2 Local)) (globalEnv, store', nextAddr) xs
          (e1, xs) = matchArgsToFunc args' (lookupVar s env store)
          (globalEnv, store') = let (Just (GlobalEnv e)) = MapL.lookup storedGlobalEnv store in 
                                 if (e == Map.empty) then (env, MapL.update (\x -> Just $ GlobalEnv env) storedGlobalEnv store) else (e, store) -- update Global Env

-- Takes in a list of evaluated arguments, and a list of evaluated function parameters.
-- Returns the function Expr value to use, as well as a list of Strings to ExprValues which need to be added to the Env and Store.
matchArgsToFunc :: [ExprValue] -> ExprValue -> (Expr, [(String, ExprValue)])
matchArgsToFunc _ (VFunc []) = error "No matching patterns for that function."
matchArgsToFunc args (VFunc ((ps,e1):xs))
    | length args /= length ps  || ys == Nothing = matchArgsToFunc args (VFunc xs)
    | otherwise = (e1, fromJust ys)
    where ys = foldr (\(p, a) acc -> let e = (matchParamToArg p a []) in 
                                        if (acc == Nothing || e == Nothing) then Nothing else Just $ (fromJust e) ++ (fromJust acc)) (Just []) (zip ps args)

          matchParamToArg (VList []) (VList []) ls = Just ls
          matchParamToArg (VList [VList []]) (VList []) ls = Just ls
          matchParamToArg (VList [VVar s]) (VList ys) ls = matchParamToArg (VVar s) (VList ys) ls

          matchParamToArg (VList (x:xs)) (VList (y:ys)) ls
                | e == Nothing = Nothing
                | otherwise = matchParamToArg (VList xs) (VList ys) (fromJust e)
                where e = matchParamToArg x y ls

          matchParamToArg (VVar s) e2 ls = Just ((s, e2):ls)
          matchParamToArg (VInt n) (VInt n') ls = if n == n' then Just ls else Nothing
          matchParamToArg (VBool b) (VBool b') ls = if b == b' then Just ls else Nothing
          matchParamToArg _ _ _ = Nothing


-- Gets the type of a Value.
getType :: ExprValue -> Type
getType (VInt _) = TInt
getType (VBool _) = TBool
getType (VList _) = TList
getType (VRef _) = TRef
getType (VStream _ _) = TStream

-- Lookup a variable in the Environment and Store. Throw an error if it can't be found, else return its corresponding ExprValue.
lookupVar :: String -> Environment -> Store -> ExprValue
lookupVar s env store
    | addr == Nothing = error $ "Value " ++ s ++ " is not in the Environment (has not been defined) -> " ++ (show env)
    | val == Nothing = error $ "Value " ++ s ++ " is not in the Store -> " ++ (show store)
    | otherwise = fromJust val
    where addr = Map.lookup s env
          val = MapL.lookup (fst $ fromJust addr) store

lookupPointerVar :: String -> Environment -> Store -> ExprValue
lookupPointerVar s env store
    | getType val == TRef = fromJust $ MapL.lookup r store
    | otherwise = error "Error, variable is not a pointer!"
    where val = lookupVar s env store
          (VRef r) = val

lookupAddr :: String -> Environment -> Address
lookupAddr s env
    | addr == Nothing = error $ "Value " ++ s ++ " is not in the Environment (has not been defined) -> " ++ (show env)
    | otherwise = fst $ fromJust addr
    where addr = Map.lookup s env

-- Binds a String to an expression, overriding the String address and scope if it already exists in the Environment.
overrideEnvStore :: Environment -> Store -> Address -> String -> ExprValue -> Scope -> (Environment, Store, Address)
overrideEnvStore env store nextAddr s e1 sc = (Map.insert s (nextAddr, sc) env, updateStore store nextAddr e1, nextAddr+1)

-- Binds a String (variable name) to an expression, updating the environment and store and returning them.
updateEnvStore :: Environment -> Store -> Address -> String -> ExprValue -> (Environment, Store, Address)
updateEnvStore env store nextAddr s e1 = (env', updateStore store addr e1, if lookupInEnv == Nothing then nextAddr+1 else nextAddr)
    where lookupInEnv = Map.lookup s env
          (env', addr) = case lookupInEnv of
                                Just (a, _) -> (env, a)
                                Nothing -> addToEnv env store nextAddr s

-- Adds a new String to the Environment, and returns a tuple of the new Environment and the created Address.
-- Assumes the String is not in the Environment.
addToEnv :: Environment -> Store -> Address -> String -> (Environment, Address)
addToEnv env store nextAddr s = (Map.insert s (addr, sc) env, addr)
    where sc = Global
          addr = case Map.lookup s env of 
                        Just (a, _) -> a
                        Nothing -> nextAddr

-- Updates an Address mapping in the store.
-- If the inputted Address is not in the store, then it will be inserted.
updateStore :: Store -> Address -> ExprValue -> Store
updateStore store a e@(VFunc xs)
    | (MapL.lookup a store) == Nothing = MapL.insert a e store
    | otherwise = MapL.update (\x -> Just (VFunc (ys ++ xs))) a store
    where (Just (VFunc ys)) = MapL.lookup a store

updateStore store a e1
    | item == Nothing = MapL.insert a e1 store
    | otherwise = MapL.update (\x -> Just e1) a store
    where item = MapL.lookup a store

-- Takes in stream num, num of values to get from the buffer, and the current store.
-- Returns updated store and list of values retrieved.
peakStreamInput :: ExprValue -> Int -> Store -> IO ([ExprValue], Store)
peakStreamInput (VStream i xs) n store = do
    (VStream i' ys, store') <- handleStreamInput (VStream i xs) n store
    return (take n ys, store')

peakStreamInput _ _ _ = error "peakStreamInput function only takes in a VStream type."

-- Takes in stream num, num of values to get from the buffer, and the current store.
-- Returns updated store and list of values retrieved.
dropStreamInput :: ExprValue -> Int -> Store -> IO ([ExprValue], Store)
dropStreamInput (VStream i xs) n store = do
    (VStream i' ys, store') <- handleStreamInput (VStream i xs) n store
    return (take n ys, MapL.update (\x -> Just (VStream i' $ drop n ys)) (-i') store')

dropStreamInput _ _ _ = error "dropStreamInput function only takes in a VStream type."

handleStreamInput :: ExprValue -> Int -> Store -> IO (ExprValue, Store)
handleStreamInput (VStream i xs) n store = do
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

    return $ case ks of 
                [] -> fst $ foldl (\(store', c) xs -> case MapL.lookup c store' of
                                            Just (VStream i ys) -> (MapL.update (\x -> Just (VStream i (ys++xs))) c store', c-1)
                                            Nothing -> (MapL.insert c (VStream (abs c) xs) store', c-1)
                                            _ -> error "Should be a VStream in negative Store addresses."
                            ) (store, 0) xss
                _ -> case (length xss)-1 < (last ks) of
                            False -> foldl (\store' k -> MapL.update (\(VStream i ys) -> Just (VStream i (ys++(xss!!k)))) (-k) store') store ks
                            True -> error "Input length is less than the specified number of input streams."

-- Read n lines of input into stream buffers (a list of lists).
readInput :: [[ExprValue]] -> Int -> IO [[ExprValue]]
readInput xss 0 = return xss
readInput [] n = do
    end <- isEOF
    line <- getLine
    let line' = foldr (\x acc -> [VInt x]:acc) [] $ map (read :: String -> Int) $ words line
    if end then return [] else readInput line' (n-1)
readInput xss n = do 
    end <- isEOF
    line <- getLine
    let line' = map ((\i -> VInt i) . (read :: String -> Int)) $ words line
    let xss' = if (length xss /= length line') 
        then (error "Error in readInput function, input is not in the correct format.") 
        else (helper xss line')
    if end 
        then return (helper xss (replicate (length xss) VNone))
        else readInput xss' (n-1)

        where helper [] [] = []
              helper (ys:yss) (x:xs) = (ys ++ [x]) : helper yss xs


-- Type error between Expr e1 and Expr e2, using operator String s, which uses type String t.
typeError :: Expr -> String -> Expr -> String -> a
typeError e1 s e2 [] = typeError e1 s e2 "the same"
typeError (Value e1) s (Value e2) t = error $ "\n\nType Error: '" ++ s ++ "' operator must be between " ++ t ++ " types, in " ++ (show e1) ++ " "++s++" " ++ (show e2) ++ 
                        ".\nThe type of expression 1 is " ++ (show $ getType e1) ++ ", but the type of expression 2 is " ++ (show $ getType e2) ++ ".\n"

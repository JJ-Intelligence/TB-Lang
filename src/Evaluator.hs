module Evaluator where
import Expression
import Parser
import Lexer
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Lazy as MapL
import Data.Maybe
import Debug.Trace

-- Reserved elements of the Store.
storedGlobalEnv = 0 -- Address of the function CallStack.
heapStart = 1 -- Starting address of the variable/function heap (space after the reserved area).

-- garbageSize = 10 -- Number of out-of-scope variables allowed in the heap before garbage collection kicks in. (NOW REDUNDANT)

-- Insert reserved items into the Store.
insertReserved :: Store -> Store
insertReserved store = MapL.insert storedGlobalEnv (GlobalEnv Map.empty) store

-- interpret :: String -> State
-- interpret s = eval $ parse $ alexScanTokens s

-- Start the evaluator by passing it an Expression (from the Parser).
startEvaluator :: Expr -> IO ()
startEvaluator e = eval $ step (e, Map.empty, insertReserved MapL.empty, heapStart, [Done])

-- Eval function encapsulates the step function, and handles its IO calls.
eval :: State -> IO ()
eval (Value v, env, store, nextAddr, (FuncCallFrame "out" env'):kon) = do
    output v env' store
    eval $ step (Value VNone, env', store, nextAddr, kon)

eval (Value (VInt n), env, store, nextAddr, (FuncCallFrame "inp" env'):kon) = do
    (val, store') <- input n 1 store
    eval $ step (Value val, env', store', nextAddr, kon) 

eval (Value (VInt n'), env, store, nextAddr, (BinOpH (BinFuncCallFrame "inp" (Value (VInt n)) env')):kon) = do
    (val, store') <- input n n' store
    eval $ step (Value val, env', store', nextAddr, kon)  

eval s@(_, _, _, _, [Done]) = putStrLn $ "\nFINISHED\n"-- ++(show s)
eval e = do
    eval $ step e

-- Step function to move from one State to another.
step :: State -> State

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

-- Accessing a variable reference.
step (Var s, env, store, nextAddr, kon) = step (Value $ lookupVar s env store, env, store, nextAddr, kon)

-- Function blocks ({ Expr }), which must have a 'return' statement.
step (FuncBlock e1, env, store, nextAddr, kon) = step (e1, env, store, nextAddr, FuncBlockFrame:kon)
step (Return e1, env, store, nextAddr, FuncBlockFrame:kon) = step (e1, env, store, nextAddr, kon)
step (Value e1, env, store, nextAddr, FuncBlockFrame:kon) = step (Value VNone, env, store, nextAddr, kon)

-- Function calls.
-- Output function.
step (FuncCall "out" (FuncParam v FuncParamEnd), env, store, nextAddr, kon) = step (v, env, store, nextAddr, (FuncCallFrame "out" env):kon)
step (FuncCall "out" _, env, store, nextAddr, kon) = error "out function only takes one parameter - a list to be printed."
step s@(Value v, env, store, nextAddr, (FuncCallFrame "out" env'):kon) = s

-- Input function.
    -- Single parameter input.
step (FuncCall "inp" (FuncParam e1 FuncParamEnd), env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (FuncCallFrame "inp" env):kon)
step s@(Value (VInt n), env, store, nextAddr, (FuncCallFrame "inp" env'):kon) = s

    -- Double parameter input.
step (FuncCall "inp" (FuncParam e1 (FuncParam e2 FuncParamEnd)), env, store, nextAddr, kon) = step (e1, env, store, nextAddr, (HBinOp (BinFuncCallFrame "inp" e2 env)):kon)
step (Value (VInt n), env, store, nextAddr, (HBinOp (BinFuncCallFrame "inp" e2 env')):kon) = step (e2, env', store, nextAddr, (BinOpH (BinFuncCallFrame "inp" (Value (VInt n)) env)):kon)
step (Value _, env, store, nextAddr, (HBinOp (BinFuncCallFrame "inp" e2 env')):kon) = error "inp function must take an int as its first parameter."
step s@(Value (VInt n), env', store, nextAddr, (BinOpH (BinFuncCallFrame "inp" (Value (VInt n')) env)):kon) = s

-- List head function.
step (FuncCall "head" (FuncParam v FuncParamEnd), env, store, nextAddr, kon) = step (v, env, store, nextAddr, (FuncCallFrame "head" env):kon)
step (FuncCall "head" _, env, store, nextAddr, kon) = error $ "head function only takes one parameter - a list. "
step (Value (VList xs), env, store, nextAddr, (FuncCallFrame "head" env'):kon)
    | length xs == 0 = error "List has no items in - head failed."
    | otherwise = (Value (head xs), env, store, nextAddr, kon)


-- List tail function.
step (FuncCall "tail" (FuncParam v FuncParamEnd), env, store, nextAddr, kon) = step (v, env, store, nextAddr, (FuncCallFrame "tail" env):kon)
step (FuncCall "tail" _, env, store, nextAddr, kon) = error "tail function only takes one parameter - a list."
step (Value (VList xs), env, store, nextAddr, (FuncCallFrame "tail" env'):kon)
    | length xs == 0 = error "List has no items in - tail failed."
    | otherwise = (Value $ VList (tail xs), env, store, nextAddr, kon)

-- List length function
step (FuncCall "length" (FuncParam v FuncParamEnd), env, store, nextAddr, kon) = step (v, env, store, nextAddr, (FuncCallFrame "length" env):kon)
step (FuncCall "length" _, env, store, nextAddr, kon) = error "length function only takes one parameter - a list."
step (Value (VList xs), env, store, nextAddr, (FuncCallFrame "length" env'):kon) = (Value (VInt (length xs)), env', store, nextAddr, kon)
step (_, env, store, nextAddr, (FuncCallFrame "length" env'):kon) = error "length function only takes one parameter - a list."

-- User-defined function calls.
step (FuncCall s ps, env, store, nextAddr, kon) = step (Value e2, env, store', nextAddr, kon) -- Continue after function call returns.
    where (e1, env', store', nextAddr') = handleFuncArgs ps env store nextAddr s -- Pattern match
          (Value e2, _, store'', _, _) = step (e1, env', store', nextAddr', ReturnFrame:kon) -- Recurse into function call.

-- Returning from a function.
step (Value e1, env, store, nextAddr, ReturnFrame:kon) = (Value e1, env, store, nextAddr, kon)

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
step (Value (VBool b'), env', store, nextAddr, (BinOpH (BinCompOp And (Value (VBool b)) env)):kon) = step (Value $ VBool $ b && b', env, store, nextAddr, kon)
step (Value e2, env', store, nextAddr, (BinOpH (BinCompOp And (Value e1) env)):kon) = typeError (Value e1) (show And) (Value e2) "Boolean"
step (Value (VBool b'), env', store, nextAddr, (BinOpH (BinCompOp Or (Value (VBool b)) env)):kon) = step (Value $ VBool $ b || b', env, store, nextAddr, kon)
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
    | otherwise = (Value VNone, env, store, nextAddr, kon)
step (Value v, env, store, nextAddr, (TerOpH (TerWhileOp c e1)):kon) = step (c, env, store, nextAddr, (HTerOp $ TerWhileOp c e1):kon)

-- For loop.
--step (While c e1, env, store, nextAddr, kon) = step (c, env, store, nextAddr, (HTerOp $ TerWhileOp c e1):kon)
--step (Value (VBool b), env, store, nextAddr, (HTerOp (TerWhileOp c e1)):kon)
--    | b = step (e1, env, store, nextAddr, (TerOpH $ TerWhileOp c e1):kon)
--    | otherwise = (Value VNone, env, store, nextAddr, kon)
--step (Value v, env, store, nextAddr, (TerOpH (TerWhileOp c e1)):kon) = step (c, env, store, nextAddr, (HTerOp $ TerWhileOp c e1):kon)

step (Value _, env, store, nextAddr, (FuncCallFrame "inp" env'):kon) = error "inp function with one parameter must take an Int."
step (Value _, env', store, nextAddr, (BinOpH (BinFuncCallFrame "inp" (Value (VInt n)) env)):kon) = error "inp function must take an int as its second parameter. "
step (_, env, store, nextAddr, (FuncCallFrame "head" env'):kon) = error $ "head function only takes one parameter - a list. "
step (_, env, store, nextAddr, (FuncCallFrame "tail" env'):kon) = error "tail function only takes one parameter - a list. "

-- End of evaluation.
step s@(_, _, _, _, [Done]) = s

-- No defined step for the current State.
step s@(exp, env, store, nextAddr, kon) = error $ "ERROR evaluating expression " ++ (show s) ++ ", no CESK step defined."


-- Pattern matches a function parameters with some given arguments, returning the functions Expr value, as well as updated Env, Store, and next Address.
-- Takes in the unevaluated arguments, the current Env, Store and next Address, as well as the function name.
handleFuncArgs :: Parameters -> Environment -> Store -> Address -> String -> (Expr, Environment, Store, Address)
handleFuncArgs args env store nextAddr s = (e1, env', store'', nextAddr')
    where (env', store'', nextAddr') = foldr (\(s, e2) (accEnv, accStore, addr) -> (overrideEnvStore accEnv accStore addr s e2 Local)) (globalEnv, store', nextAddr) xs
          (e1, xs) = matchArgsToFunc args' (lookupVar s env store)
          args' = evaluateArgs args env store nextAddr
          (globalEnv, store') = let (Just (GlobalEnv e)) = MapL.lookup storedGlobalEnv store in 
                                 if (e == Map.empty) then (env, MapL.update (\x -> Just $ GlobalEnv env) storedGlobalEnv store) else (e, store) -- update Global Env

          evaluateArgs FuncParamEnd env store nextAddr = []
          evaluateArgs (FuncParam e1 e2) env store nextAddr = e1' : evaluateArgs e2 env store nextAddr
                where (Value e1',_,_,_,_) = step (e1, env, store, nextAddr, [Done])

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

-- Lookup a variable in the Environment and Store. Throw an error if it can't be found, else return its corresponding ExprValue.
lookupVar :: String -> Environment -> Store -> ExprValue
lookupVar s env store
    | addr == Nothing = error $ "Value " ++ s ++ " is not in the Environment (has not been defined) -> " ++ (show env)
    | val == Nothing = error $ "Value " ++ s ++ " is not in the Store -> " ++ (show store)
    | otherwise = fromJust val
    where addr = Map.lookup s env
          val = MapL.lookup (fst $ fromJust addr) store

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

-- Output function. Prints a list of values to stdout.
output :: ExprValue -> Environment -> Store -> IO ()
output v env store = putStr (show v)
-- output _ _ _ = error "Invalid arguments for 'out' function, it only takes in a List type."

-- readInputWrapper :: Int -> Store -> (Int, Store)
-- readInputWrapper streamI store
--     | length $ buffer > streamI || (length $ buffer) == 0 = (head newBuffer,  updateStore 1 (VInputBuffer newBuffer) store)
--     | otherwise = (head buffer, updateStore 1 (VInputBuffer ) store)
--     where
--         buffer = buffers !! streamI
--         buffers = removeVInputBuffer (lookup 1 store)
--         newBuffer = readInput buffers 1
--         removeVInputBuffer (VInputBuffer e) = e

-- Input function. Reads in n values from a given sequence.
-- Takes in the functions parameters, the current Environment and Store, returning an ExprValue and the updated Store (containing updated buffers (VLists))
input :: Int -> Int -> Store -> IO (ExprValue, Store)
input seqNo n store = return (VNone, store)

-- Read n lines of input into stream buffers (a list of lists).
readInput :: [[Int]] -> Int -> IO [[Int]]
readInput xs 0 = return xs
readInput [] n
    | n > 0 = do
        line <- getLine
        let line' = foldr (\x acc -> [x]:acc) [] $ map (read :: String -> Int) $ words line
        readInput line' (n-1)
    | otherwise = error "Error in readInput function, must read at least one line of input."
readInput xs n = do 
    line <- getLine
    let line' = map (read :: String -> Int) $ words line
    let xs' = if (length xs /= length line') then (error "Error in readInput function, input is not in the correct format.") else (helper xs line')
    readInput xs' (n-1)
        where helper [] [] = []
              helper (ys:yss) (x:xs) = (x:ys) : helper yss xs


-- Type error between Expr e1 and Expr e2, using operator String s, which uses type String t.
typeError :: Expr -> String -> Expr -> String -> a
typeError e1 s e2 [] = typeError e1 s e2 "the same"
typeError (Value e1) s (Value e2) t = error $ "\n\nType Error: '" ++ s ++ "' operator must be between " ++ t ++ " types, in " ++ (show e1) ++ " "++s++" " ++ (show e2) ++ 
                        ".\nThe type of expression 1 is " ++ (show $ getType e1) ++ ", but the type of expression 2 is " ++ (show $ getType e2) ++ ".\n"

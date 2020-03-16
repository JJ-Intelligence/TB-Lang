module Evaluator where
import Expression
import Parser
import Lexer
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Lazy as MapL
import Data.Maybe
import Debug.Trace

-- Reserved elements of the Store.
funcCallStack = 0 -- Address of the function CallStack.
heapStart = 1 -- Starting address of the variable/function heap (space after the reserved area).

-- garbageSize = 10 -- Number of out-of-scope variables allowed in the heap before garbage collection kicks in. (NOW REDUNDANT)

-- Insert reserved items into the Store.
insertReserved :: Store -> Store
insertReserved store = MapL.insert funcCallStack (CallStack [] Map.empty) store

-- interpret :: String -> State
-- interpret s = eval $ parse $ alexScanTokens s

-- Start the evaluator by passing it an Expression (from the Parser).
startEvaluator :: Expr -> IO ()
startEvaluator e = eval $ step (e, Map.empty, insertReserved MapL.empty, [Done])

-- Eval function encapsulates the step function, and handles its IO calls.
eval :: State -> IO ()
eval (Value v, env, store, (FuncCallFrame "out" env'):kon) = do
    output v env' store
    eval $ step (Value VNone, env', store, kon)

eval (Value (VInt n), env, store, (FuncCallFrame "inp" env'):kon) = do
    (val, store') <- input n 1 store
    eval $ step (Value val, env', store', kon) 

eval (Value (VInt n'), env, store, (BinOpH (BinFuncCallFrame "inp" (Value (VInt n)) env')):kon) = do
    (val, store') <- input n n' store
    eval $ step (Value val, env', store', kon)  

eval s@(_, _, _, [Done]) = putStrLn $ "\n"-- ++(show s)
eval e = do
    eval $ step e

-- Step function to move from one State to another.
step :: State -> State

-- Converting Literals to Values.
step (Literal (EInt n), env, store, kon) = step (Value $ VInt n, env, store, kon)
step (Literal (EBool n), env, store, kon) = step (Value $ VBool n, env, store, kon)
step (Literal Empty, env, store, kon) = step (Value $ VList [], env, store, kon)
step (Literal ENone, env, store, kon) = step (Value VNone, env, store, kon)

-- Sequence operation ';'.
step (Seq e1 e2, env, store, kon) = step (e1, env, store, (HBinOp $ BinSeqOp e2):kon)
step (Value e1, env, store, (HBinOp (BinSeqOp e2)):kon) = step (e2, env, store, kon)
    -- where store' = garbageCollection env store -- garbage collection

-- Defining a new Function.
step (DefVar s (Func ps e1), env, store, kon) = step (Value $ VFunc [(ps, e1)], env', store', kon)
    where (env', store') = updateEnvStore env store s (VFunc [(ps, e1)])

-- Defining a new Var.
-- Looks for the variable in the Env, and replaces it in the Store if it exists, else it creates it.
step (DefVar s e1, env, store, kon) = step (e1, env, store, (DefVarFrame s env):kon)
step (Value e1, env, store, (DefVarFrame s env'):kon) = step (Value e1, env'', store', kon)
    where (env'', store') = updateEnvStore env' store s e1

-- Accessing a variable reference.
step (Var s, env, store, kon) = step (Value $ lookupVar s env store, env, store, kon)

-- Function blocks ({ Expr }), which must have a 'return' statement.
step (FuncBlock e1, env, store, kon) = step (e1, env, store, FuncBlockFrame:kon)
step (Return e1, env, store, FuncBlockFrame:kon) = (e1, env, store, kon)
step (Value e1, env, store, FuncBlockFrame:kon) = (Value VNone, env, store, kon)

-- Function calls.
-- Output function.
step (FuncCall "out" (FuncParam v FuncParamEnd), env, store, kon) = step (v, env, store, (FuncCallFrame "out" env):kon)
step (FuncCall "out" _, env, store, kon) = error "out function only takes one parameter - a list to be printed."
step s@(Value v, env, store, (FuncCallFrame "out" env'):kon) = s

-- Input function.
    -- Single parameter input.
step (FuncCall "inp" (FuncParam e1 FuncParamEnd), env, store, kon) = step (e1, env, store, (FuncCallFrame "inp" env):kon)
step s@(Value (VInt n), env, store, (FuncCallFrame "inp" env'):kon) = s
step (Value _, env, store, (FuncCallFrame "inp" env'):kon) = error "inp function with one parameter must take an Int."

    -- Double parameter input.
step (FuncCall "inp" (FuncParam e1 (FuncParam e2 FuncParamEnd)), env, store, kon) = step (e1, env, store, (HBinOp (BinFuncCallFrame "inp" e2 env)):kon)
step (Value (VInt n), env, store, (HBinOp (BinFuncCallFrame "inp" e2 env')):kon) = step (e2, env', store, (BinOpH (BinFuncCallFrame "inp" (Value (VInt n)) env)):kon)
step (Value _, env, store, (HBinOp (BinFuncCallFrame "inp" e2 env')):kon) = error "inp function must take an int as its first parameter."
step s@(Value (VInt n), env', store, (BinOpH (BinFuncCallFrame "inp" (Value (VInt n')) env)):kon) = s
step (Value _, env', store, (BinOpH (BinFuncCallFrame "inp" (Value (VInt n)) env)):kon) = error "inp function must take an int as its second parameter."

-- List head function.
step (FuncCall "head" (FuncParam v FuncParamEnd), env, store, kon) = step (v, env, store, (FuncCallFrame "head" env):kon)
step (FuncCall "head" _, env, store, kon) = error "head function only takes one parameter - a list."
step (Value (VList xs), env, store, (FuncCallFrame "head" env'):kon)
    | length xs == 0 = (Value VNone, env, store, kon) -- Safe head, because we don't hate people <3
    | otherwise = (Value (head xs), env, store, kon)
step (_, env, store, (FuncCallFrame "head" env'):kon) = error "head function only takes one parameter - a list."

-- List tail function.
step (FuncCall "tail" (FuncParam v FuncParamEnd), env, store, kon) = step (v, env, store, (FuncCallFrame "tail" env):kon)
step (FuncCall "tail" _, env, store, kon) = error "tail function only takes one parameter - a list."
step (Value (VList xs), env, store, (FuncCallFrame "tail" env'):kon)
    | length xs == 0 = (Value (VList xs), env, store, kon) -- Safe tail, because we don't hate people <3
    | otherwise = (Value $ VList (tail xs), env, store, kon)
step (_, env, store, (FuncCallFrame "tail" env'):kon) = error "head function only takes one parameter - a list."

-- List length function
step (FuncCall "length" (FuncParam v FuncParamEnd), env, store, kon) = step (v, env, store, (FuncCallFrame "length" env):kon)
step (FuncCall "length" _, env, store, kon) = error "length function only takes one parameter - a list."
step (Value (VList xs), env, store, (FuncCallFrame "length" env'):kon) = (Value (VInt (length xs)), env', store, kon)
step (_, env, store, (FuncCallFrame "length" env'):kon) = error "length function only takes one parameter - a list."

-- User-defined function calls.
step (FuncCall s ps, env, store, kon) = step (e1, env', store'', ReturnFrame:kon)
    where val = lookupVar s env store
          store' = updateStore store funcCallStack (CallStack [ env' ] env)
          (e1, env', store'') = matchFuncPattern ps val env store'

-- Returning from a function.
step (Value e1, env, store, ReturnFrame:kon)
    | length xs == 0 = step (Value e1, y, store, kon)
    | otherwise = step (Value e1, env', store, kon)
    where (Just (CallStack xs y)) = MapL.lookup funcCallStack store
          env' = head xs


-- Math binary operations.
step (Op (MathOp op e1 e2), env, store, kon) = step (e1, env, store, (HBinOp $ BinMathOp op e2 env):kon)
step (Value e1, env, store, (HBinOp (BinMathOp op e2 env')):kon) = step (e2, env', store, (BinOpH $ BinMathOp op (Value e1) env):kon)
step (Value (VInt n'), env', store, (BinOpH (BinMathOp op (Value (VInt n)) env)):kon) = step (Value $ VInt r, env, store, kon)
    where r = case op of
                    Plus -> n + n'
                    Min -> n - n'
                    Mult -> n * n'
                    Div -> n `div` n'
                    Exp -> n ^ n'
                    Mod -> n `mod` n'
step (Value (VList n'), env', store, (BinOpH (BinMathOp Plus (Value (VList n)) env)):kon)
    | getType (VList (n ++ n')) /= TConflict = step (Value $ VList (n ++ n'), env, store, kon)
    | otherwise = typeError (Value (VList n)) (show Plus) (Value (VList n')) []
step (Value e2, env', store, (BinOpH (BinMathOp op (Value e1) env)):kon) = typeError (Value e1) (show op) (Value e2) "Integer"

-- Binary comparison operations: ==, &&, ||, <, >
step (Op (CompOp op e1 e2), env, store, kon) = step (e1, env, store, (HBinOp $ BinCompOp op e2 env):kon)
step (Value e1, env, store, (HBinOp (BinCompOp op e2 env')):kon) = step (e2, env', store, (BinOpH $ BinCompOp op (Value e1) env):kon)

-- Boolean &&, || operations.
step (Value (VBool b'), env', store, (BinOpH (BinCompOp And (Value (VBool b)) env)):kon) = step (Value $ VBool $ b && b', env, store, kon)
step (Value e2, env', store, (BinOpH (BinCompOp And (Value e1) env)):kon) = typeError (Value e1) (show And) (Value e2) "Boolean"
step (Value (VBool b'), env', store, (BinOpH (BinCompOp Or (Value (VBool b)) env)):kon) = step (Value $ VBool $ b || b', env, store, kon)
step (Value e2, env', store, (BinOpH (BinCompOp Or (Value e1) env)):kon) = typeError (Value e1) (show Or) (Value e2) "Boolean"

-- Comparison ==, <, > operations.
step (Value (VInt n'), env', store, (BinOpH (BinCompOp LessThan (Value (VInt n)) env)):kon) = step (Value $ VBool $ n < n', env, store, kon)
step (Value e2, env', store, (BinOpH (BinCompOp LessThan (Value e1) env)):kon) = typeError (Value e1) (show LessThan) (Value e2) "Integer"
step (Value (VInt n'), env', store, (BinOpH (BinCompOp GreaterThan (Value (VInt n)) env)):kon) = step (Value $ VBool $ n > n', env, store, kon)
step (Value e2, env', store, (BinOpH (BinCompOp GreaterThan (Value e1) env)):kon) = typeError (Value e1) (show GreaterThan) (Value e2) "Integer"

step (Value e2, env', store, (BinOpH (BinCompOp Equality (Value e1) env)):kon)
    | getType e1 == getType e2 = step (Value $ VBool $ e1 == e2, env, store, kon)
    | otherwise = typeError (Value e1) (show Equality) (Value e2) []

-- Cons binary operation.
step (Op (Cons e1 e2), env, store, kon) = step (e1, env, store, (HBinOp $ BinConsOp e2 env):kon)
step (Value e1, env, store, (HBinOp (BinConsOp e2 env')):kon) = step (e2, env', store, (BinOpH (BinConsOp (Value e1) env')):kon)
step (Value (VList xs), env, store, (BinOpH (BinConsOp (Value e1) env')):kon) = step (Value (VList (e1:xs)), env', store, kon)

-- if-elif-else statement.
step (If c e1 e2, env, store, kon) = step (c, env, store, (HTerOp $ TerIfOp e1 e2):kon)
step (Value (VBool b), env, store, (HTerOp (TerIfOp e1 e2)):kon)
    | b = step (e1, env, store, kon)
    | otherwise = step $ helper e2
    where helper Nothing = (Value VNone, env, store, kon)
          helper (Just (Else e)) = (e, env, store, kon)
          helper (Just (Elif c' e1' e2')) = (If c' e1' e2', env, store, kon)

-- While loop.
step (While c e1, env, store, kon) = step (c, env, store, (HTerOp $ TerWhileOp c e1):kon)
step (Value (VBool b), env, store, (HTerOp (TerWhileOp c e1)):kon)
    | b = step (e1, env, store, (TerOpH $ TerWhileOp c e1):kon)
    | otherwise = (Value VNone, env, store, kon)
step (Value v, env, store, (TerOpH (TerWhileOp c e1)):kon) = step (c, env, store, (HTerOp $ TerWhileOp c e1):kon)

-- End of evaluation.
step s@(_, _, _, [Done]) = s

-- No defined step for the current State.
step s@(exp, env, store, kon) = error $ "ERROR evaluating expression " ++ (show s) ++ ", no CESK step defined."

-- Match parameters to a function, returning the matched functions expression and the new environment and store containing the bound
-- parameter variables. Throws an error if no function could be matched.
matchFuncPattern :: Parameters -> ExprValue -> Environment -> Store -> (Expr, Environment, Store)
matchFuncPattern _ (VFunc []) _ _ = error "No matching patterns for that function."
matchFuncPattern ps (VFunc ((ps',e1):xs)) env store
    | e == Nothing = matchFuncPattern ps (VFunc xs) env store
    | otherwise = (e1, env'', store')
    where e = patternMatch ps ps' env store []
          (Just (env', ls, store')) = e
          env'' = Map.filterWithKey (\k (a,sc) -> a < heapStart || sc == Global || k `elem` ls) env' -- Clear the local scope of the calling functions variables.

-- Match inputted parameters (values) with function parameters (not values - e.g. cons operation)
-- If the function parameters couldn't be matched, then return Nothing.
patternMatch :: Parameters -> Parameters -> Environment -> Store -> [String] -> Maybe (Environment, [String], Store)
patternMatch FuncParamEnd FuncParamEnd env store ls = Just (env, ls, store)
patternMatch FuncParamEnd _ _ _ _ = Nothing
patternMatch _ FuncParamEnd _ _ _ = Nothing
patternMatch (FuncParam e1 xs) (FuncParam y ys) env store ls
    | e == Nothing = Nothing
    | otherwise = patternMatch xs ys env' store' ls'
    where (Value x,_,_,_) = step (e1, env, store, [Done])
          e = matchExprs x y env store ls
          (Just (env', ls', store')) = e

-- Match an ExprValue to an Expr, and return the updated Environment and Store as a Maybe type.
-- If the expression couldn't be matched, then return Nothing.
matchExprs :: ExprValue -> Expr -> Environment -> Store -> [String] -> Maybe (Environment, [String], Store)
matchExprs (VList []) (Literal Empty) env store ls = Just (env, ls, store)

matchExprs (VList xs) (Var s) env store ls = Just (env', s:ls, store')
    where (env', store') = overrideEnvStore env store s (VList xs) Local

matchExprs (VList (x:xs)) (Op (Cons e1 e2)) env store ls
    | e == Nothing = Nothing
    | otherwise = matchExprs (VList xs) e2 env' store' ls'
    where e = matchExprs x e1 env store ls
          (Just (env', ls', store')) = e

matchExprs e1 (Var s) env store ls = Just (env', s:ls, store')
    where (env', store') = overrideEnvStore env store s e1 Local

matchExprs (VInt n) (Literal (EInt n')) env store ls
    | n == n' = Just (env, ls, store)
    | otherwise = Nothing

matchExprs (VBool b) (Literal (EBool b')) env store ls
    | b == b' = Just (env, ls, store)
    | otherwise = Nothing

matchExprs _ _ _ _ _ = Nothing

-- Gets the type of a Value, returning TConflict if the Value has conflicting types.
getType :: ExprValue -> Type
getType (VInt _) = TInt
getType (VBool _) = TBool
getType (VList _) = TList

-- Lookup a variable in the Environment and Store. Throw an error if it can't be found, else return its corresponding ExprValue.
lookupVar :: String -> Environment -> Store -> ExprValue
lookupVar s env store
    | addr == Nothing = error $ "Value " ++ s ++ " is not in the Environment (has not been defined)."
    | val == Nothing = error $ "Value " ++ s ++ " is not in the Store."
    | otherwise = fromJust val
    where addr = Map.lookup s env
          val = MapL.lookup (fst $ fromJust addr) store

-- Binds a String to an expression, overriding the String address and scope if it already exists in the Environment.
overrideEnvStore :: Environment -> Store -> String -> ExprValue -> Scope -> (Environment, Store)
overrideEnvStore env store s e1 sc = (Map.insert s (addr, sc) env, updateStore store addr e1)
    where addr = findNextAvailableAddr store heapStart

-- Binds a String (variable name) to an expression, updating the environment and store and returning them.
updateEnvStore :: Environment -> Store -> String -> ExprValue -> (Environment, Store)
updateEnvStore env store s e1 = (env', updateStore store addr e1)
    where (env', addr) = case Map.lookup s env of
                                Just (a, _) -> (env, a)
                                Nothing -> addToEnv env store s

-- Adds a new String to the Environment, and returns a tuple of the new Environment and the created Address.
-- Assumes the String is not in the Environment.
addToEnv :: Environment -> Store -> String -> (Environment, Address)
addToEnv env store s = (Map.insert s (addr, sc) env, addr)
    where (Just (CallStack xs y)) = MapL.lookup funcCallStack store
          sc = if (length xs > 0) then Local else Global
          addr = case Map.lookup s env of 
                        Just (a, _) -> a
                        Nothing -> (findNextAvailableAddr store heapStart)

-- Find the next available free address in the Store.
findNextAvailableAddr :: Store -> Int -> Int
findNextAvailableAddr store c
    | MapL.lookup c store == Nothing = c
    | otherwise = findNextAvailableAddr store (c+1)

-- Updates an Address mapping in the store.
-- If the inputted Address is not in the store, then it will be inserted.
updateStore :: Store -> Address -> ExprValue -> Store
updateStore store a e@(CallStack xs y) = MapL.update (\x -> Just (CallStack (xs ++ ys) globalEnv)) a store
    where (Just (CallStack ys y')) = MapL.lookup a store
          globalEnv = if (y' == Map.empty) then y else y'
updateStore store a e@(VFunc xs)
    | (MapL.lookup a store) == Nothing = MapL.insert a e store
    | otherwise = MapL.update (\x -> Just (VFunc (ys ++ xs))) a store
    where (Just (VFunc ys)) = MapL.lookup a store

updateStore store a e1
    | item == Nothing = MapL.insert a e1 store
    | otherwise = MapL.update (\x -> Just e1) a store
    where item = MapL.lookup a store

-- Calls up the bin man to collect the garbage.
-- Filters the Store so it only contains addresses also in the Environment, and anything below the heapStart address (contians things such as the CallStack).
-- It only collects garbage if the amount of garbage is greater than 'garbageSize'.
-- garbageCollection :: Environment -> Store -> Store -- GARBAGE COLLECTION IS NOW REDUNDANT (as scope handles variable deletion)
-- garbageCollection env store
--     | ((Map.size store) - heapStart) - (Map.size env) > garbageSize = Map.filterWithKey (\k v -> k < heapStart || k `elem` (Map.elems env)) store
--     | otherwise = store

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

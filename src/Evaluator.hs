module Evaluator where
import Expression
import Parser
import Lexer
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace

-- Reserved elements of the Store.
funcCallStack = 0

insertReserved :: Store -> Store
insertReserved store = Map.insert funcCallStack (CallStack []) Map.empty


interpret :: String -> State
interpret s = eval $ parse $ alexScanTokens s

-- Evaluation function to take an Expression (Control) and run it on the finite state machine.
eval :: Expr -> State
eval e = step (e, Map.empty, insertReserved Map.empty, [Done])


-- Step function to move from one State to another.
step :: State -> State

-- Converting Literals to Values.
step (Literal (EInt n), env, store, kon) = step (Value $ VInt n, env, store, kon)
step (Literal (EBool n), env, store, kon) = step (Value $ VBool n, env, store, kon)
step (Literal Empty, env, store, kon) = step (Value $ VList [], env, store, kon)

-- Sequence operation ';'.
step (Seq e1 e2, env, store, kon) = step (e1, env, store, (HBinOp $ BinSeqOp e2):kon)
step (Value e1, env, store, (HBinOp (BinSeqOp e2)):kon) = step (e2, env, store, kon)

-- Defining a new Function.
step (DefVar s (Func ps e1), env, store, kon) = step (Value VNone, env', store', kon)
    where (env', store') = updateEnvStore env store s (VFunc [(ps, e1)])

-- Defining a new Var.
-- Looks for the variable in the Env, and replaces it in the Store if it exists, else it creates it.
step (DefVar s e1, env, store, kon) = step (e1, env, store, (DefVarFrame s env):kon)

step (Value e1, env, store, (DefVarFrame s env'):kon) = step (Value VNone, env'', store', kon)
    where (env'', store') = updateEnvStore env' store s e1

-- Accessing a variable reference.
step (Var s, env, store, kon) 
    | addr == Nothing = error $ "Variable " ++ s ++ " is not in the Environment (has not been defined)."
    | val == Nothing = error $ "Variable " ++ s ++ " is not in the Store."
    | otherwise = step (Value $ fromJust val, env, store, kon)
    where addr = Map.lookup s env
          val = Map.lookup (fromJust addr) store

-- Function call.
step (FuncCall s ps, env, store, kon)
    | addr == Nothing = error $ "Function " ++ s ++ " is not in the Environment (has not been defined)."
    | val == Nothing = error $ "Function " ++ s ++ " is not in the Store."
    | otherwise = step (e1, env', store'', kon)
    where store' = updateStore store funcCallStack (CallStack [ (env, kon) ])
          (e1, env', store'') = matchFuncPattern ps (fromJust val) env store'
          addr = Map.lookup s env
          val = Map.lookup (fromJust addr) store

-- Returning from a function.
step (Return e1, env, store, kon) = step (e1, env, store, ReturnFrame:kon)
step (Value e1, env, store, ReturnFrame:kon)
    | length xs == 0 = error "Nothing on the CallStack, so can't return!"
    | otherwise = step (Value e1, env', store', kon')
    where (Just (CallStack xs)) = Map.lookup funcCallStack store
          (env', kon') = head xs
          store' = Map.update (\x -> Just (CallStack (tail xs))) funcCallStack store

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

step (Value (VList []), env, store, (BinOpH (BinConsOp (Value e1) env')):kon) = step (Value $ VList [e1], env', store, kon) -- Don't think we need this line

step (Value (VList xs), env, store, (BinOpH (BinConsOp (Value e1) env')):kon)
    | getType (VList (e1:xs)) /= TConflict = step (Value (VList (e1:xs)), env', store, kon)
    | otherwise = typeError (Value e1) ":" (Value $ VList xs) []

-- if-elif-else statement.
step (If c e1 e2, env, store, kon) = step (c, env, store, (HTerOp $ TerIfOp e1 e2 env):kon)
step (Value (VBool b), env, store, (HTerOp (TerIfOp e1 e2 env')):kon)
    | b = step (e1, env', store, kon)
    | otherwise = step $ helper e2
    where helper Nothing = (Value VNone, env', store, kon)
          helper (Just (Else e)) = (e, env', store, kon)
          helper (Just (Elif c' e1' e2')) = (If c' e1' e2', env', store, kon)

-- End of evaluation.
step s@(_, _, _, [Done]) = s

-- No defined step for the current State.
step (exp, env, store, kon) = error $ "ERROR evaluating expression " ++ (show exp) ++ ", no CESK step defined."

-- Match parameters to a function
matchFuncPattern :: Parameters -> ExprValue -> Environment -> Store -> (Expr, Environment, Store)
matchFuncPattern _ (VFunc []) _ _ = error "No matching patterns for that function."
matchFuncPattern ps (VFunc ((ps',e1):xs)) env store
    | e == Nothing = matchFuncPattern ps (VFunc xs) env store
    | otherwise = (e1, env', store')
    where e = patternMatch ps ps' env store
          (Just (env', store')) = e

-- Match inputted parameters (values) with function parameters (not values - e.g. cons operation)
patternMatch :: Parameters -> Parameters -> Environment -> Store -> Maybe (Environment, Store)
patternMatch FuncParamEnd FuncParamEnd env store = Just (env, store)
patternMatch FuncParamEnd _ _ _ = Nothing
patternMatch _ FuncParamEnd _ _ = Nothing
patternMatch (FuncParam e1 xs) (FuncParam y ys) env store
    | e == Nothing = Nothing
    | otherwise = patternMatch xs ys env' store'
    where (Value x,_,_,_) = step (e1, env, store, [Done])
          e = matchExprs x y env store
          (Just (env', store')) = e

matchExprs :: ExprValue -> Expr -> Environment -> Store -> Maybe (Environment, Store)
matchExprs (VList []) (Literal Empty) env store = Just (env, store)
matchExprs (VList xs) (Var s) env store = Just (env', store')
    where (env', store') = updateEnvStore env store s (VList xs)

matchExprs (VList (x:xs)) (Op (Cons e1 e2)) env store
    | e == Nothing = Nothing
    | otherwise = matchExprs (VList xs) e2 env' store'
    where e = matchExprs x e1 env store
          (Just (env', store')) = e

matchExprs e1 (Var s) env store = Just $ updateEnvStore env store s e1
matchExprs (VInt n) (Literal (EInt n')) env store
    | n == n' = Just (env, store)
    | otherwise = Nothing
matchExprs (VBool b) (Literal (EBool b')) env store
    | b == b' = Just (env, store)
    | otherwise = Nothing
matchExprs _ _ _ _ = Nothing

-- Gets the type of a Value, returning TConflict if the Value has conflicting types.
getType :: ExprValue -> Type
getType (VInt _) = TInt
getType (VBool _) = TBool
getType (VList []) = TList TEmpty
getType (VList xs)
    | length m == 0 = TList TEmpty
    | length m == 1 = if (head m) == TConflict then TConflict else TList $ head m
    | r == TConflict = TConflict
    | otherwise = TList r
    where m = map (getType) xs
          r = helper (tail m) (head m)
          helper [] t = t
          helper (TInt:xs) TInt = helper xs TInt
          helper (TInt:xs) TEmpty = helper xs TInt
          helper (TBool:xs) TBool = helper xs TBool
          helper (TBool:xs) TEmpty = helper xs TBool
          helper (TEmpty:xs) acc = helper xs acc
          helper e@((TList x):xs) (TList y)
              | r' == TConflict = TConflict
              | otherwise = TList r'
              where r' = helper (helper' e) y
                    helper' [] = []
                    helper' (TEmpty:xs) = xs
                    helper' ((TList x):xs) = x : helper' xs
                    helper' _ = [TConflict]
          helper _ _ = TConflict


-- Binds a String (variable name) to an expression, updating the environment and store and returning them.
updateEnvStore :: Environment -> Store -> String -> ExprValue -> (Environment, Store)
updateEnvStore env store s e1 = (env', updateStore store addr e1)
    where (env', addr) = case Map.lookup s env of
                                Just (a) -> (env, a)
                                Nothing -> addToEnv env store s

-- Adds a new String to the Environment, and returns a tuple of the new Environment and the created Address.
addToEnv :: Environment -> Store -> String -> (Environment, Address)
addToEnv env store s 
    | Map.lookup s env == Nothing = (Map.insert s addr env, addr)
    | otherwise = (env, addr)
    where addr = case Map.lookup s env of 
                        Just (a) -> a
                        Nothing -> (helper store 0)

          helper store c
                | Map.lookup c store == Nothing = c
                | otherwise = helper store (c+1)

-- Updates an Address mapping in the store.
-- If the inputted Address is not in the store, then it will be inserted.
updateStore :: Store -> Address -> ExprValue -> Store
updateStore store a e@(CallStack xs) = Map.update (\x -> Just (CallStack (xs ++ ys))) a store
    where (Just (CallStack ys)) = Map.lookup a store
updateStore store a e@(VFunc xs)
    | (Map.lookup a store) == Nothing = Map.insert a e store
    | otherwise = Map.update (\x -> Just (VFunc (ys ++ xs))) a store
    where (Just (VFunc ys)) = Map.lookup a store

updateStore store a e1
    | item == Nothing = Map.insert a e1 store
    | otherwise = Map.update (\x -> Just e1) a store
    where item = Map.lookup a store

-- readInputWrapper :: Int -> Store -> (Int, Store)
-- readInputWrapper streamI store
--     | length $ buffer > streamI || (length $ buffer) == 0 = (head newBuffer,  updateStore 1 (VInputBuffer newBuffer) store)
--     | otherwise = (head buffer, updateStore 1 (VInputBuffer ) store)
--     where
--         buffer = buffers !! streamI
--         buffers = removeVInputBuffer (lookup 1 store)
--         newBuffer = readInput buffers 1
--         removeVInputBuffer (VInputBuffer e) = e

output :: ExprValue -> IO ()
output v = do
    print (show v)

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




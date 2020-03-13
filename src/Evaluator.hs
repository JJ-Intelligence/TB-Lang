module Evaluator where
import Parser
import Lexer
import qualified Data.Map.Strict as Map
import Data.Maybe

-- Environment - A mapping of functions and variables to Closures (which maps an Expression to an Environment).
type Address = Int
type Environment = Map.Map String Address
type Store = Map.Map Address Expr

-- Kontinuation - A stack containing Frames showing what to do.
type Kon = [ Frame ]

-- Frame - Data structures to be put onto the Kontinuation.
data BinOpFrame = BinCompOp ExprComp Expr Environment -- Frame for a binary comparison operation - e.g. [-] == e2
                | BinSeqOp Expr -- Frame for a binary sequence operation - e.g. [-] ; e2
                | BinConsOp Expr Environment
                | BinMathOp ExprMath Expr Environment
                deriving (Show)

data TerOpFrame = TerIfOp Expr (Maybe ExprElif) Environment -- Frame for a ternary if statement operation - e.g. if [-] then e1 e2 (e2 is the else/elif)
                deriving (Show)

data Frame = HBinOp BinOpFrame
           | BinOpH BinOpFrame
           | HTerOp TerOpFrame
           | TerOpH TerOpFrame
           | DefVarFrame String Environment
           | Done 
           deriving (Show)

-- State - The current state/configuration of the CESK machine.
type State = (Expr, Environment, Store, Kon)

-- Data types.
data Type = TInt 
          | TBool 
          | TEmpty 
          | TList Type 
          | TConflict 
          deriving (Eq)

instance Show Type where 
    show TInt = "Int"
    show TBool = "Boolean"
    show TEmpty = ""
    show (TList t) = "[" ++ (show t) ++ "]" 
    show TConflict = "Conflict"

interpret :: String -> State
interpret s = eval $ parse $ alexScanTokens s

-- Evaluation function to take an Expression (Control) and run it on the finite state machine.
eval :: Expr -> State
eval e = step (e, Map.empty, Map.empty, [Done])


-- Step function to move from one State to another.
step :: State -> State

-- Converting Literals to Values.
step (Literal (EInt n), env, store, kon) = step (Value $ VInt n, env, store, kon)
step (Literal (EBool n), env, store, kon) = step (Value $ VBool n, env, store, kon)
step (Literal Empty, env, store, kon) = step (Value $ VList [], env, store, kon)

-- Sequence operation ';'.
step (Seq e1 e2, env, store, kon) = step (e1, env, store, (HBinOp $ BinSeqOp e2):kon)
step (Value e1, env, store, (HBinOp (BinSeqOp e2)):kon) = step (e2, env, store, kon)

-- Defining a new Var.
-- Looks for the variable in the Env, and replaces it in the Store if it exists, else it creates it.
step (DefVar s e1, env, store, kon) = step (e1, env, store, (DefVarFrame s env):kon)

step (Value e1, env, store, (DefVarFrame s env'):kon) = step (Value e1, env'', store', kon)
    where (env'', store') = updateEnvStore env' store s (Value e1)

-- Accessing a variable reference.
step (Var s, env, store, kon) 
    | addr == Nothing = error $ "Variable " ++ s ++ " is not in the Environment (has not been defined)."
    | val == Nothing = error $ "Variable " ++ s ++ " is not in the Store."
    | otherwise = step (fromJust val, env, store, kon)
    where addr = Map.lookup s env
          val = Map.lookup (fromJust addr) store

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
updateEnvStore :: Environment -> Store -> String -> Expr -> (Environment, Store)
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
updateStore :: Store -> Address -> Expr -> Store
updateStore store a e1
    | item == Nothing = Map.insert a e1 store
    | otherwise = Map.update (\x -> Just e1) a store
    where item = Map.lookup a store

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
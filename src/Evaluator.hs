module Evaluator where
import Parser
import qualified Data.Map.Strict as Map
import Data.Maybe

-- Environment - A mapping of functions and variables to Closures (which maps an Expression to an Environment).
type Address = Int
type Environment = Map.Map String Address
type Store = Map.Map Address Expr

-- Kontinuation - A stack containing Frames showing what to do.
type Kon = [ Frame ]

-- Frame - Data structures to be put onto the Kontinuation.
data BinOpFrame = BinCompOp ExprComp Expr -- Frame for a binary comparison operation - e.g. [-] == e2
                | BinSeqOp Expr -- Frame for a binary sequence operation - e.g. [-] ; e2
                | BinConsOp Expr Environment
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

data Type = TInt | TBool | TEmpty | TList Type | TConflict deriving (Eq, Show)

-- Evaluation function to take an Expression (Control) and run it on the finite state machine.
eval :: Expr -> State
eval e = step (e, Map.empty, Map.empty, [Done])


-- Step function to move from one State to another.
step :: State -> State

-- Converting Literals to Values.
step (Literal (EInt n), env, store, kon) = step (Value $ VInt n, env, store, kon)
step (Literal (EBool n), env, store, kon) = step (Value $ VBool n, env, store, kon)
step (Literal Empty, env, store, kon) = step (Value $ VList [], env, store, kon)

step (Seq e1 e2, env, store, kon) = step (e1, env, store, (HBinOp $ BinSeqOp e2):kon)
step (Value e1, env, store, (HBinOp (BinSeqOp e2)):kon) = step (e2, env, store, kon)

-- Defining a new Var.
-- Looks for the variable in the Env, and replaces it in the Store if it exists, else it creates it.
step (DefVar s e1, env, store, kon) = step (e1, env, store, (DefVarFrame s env):kon)

step (Value e1, env, store, (DefVarFrame s env'):kon) = step (Value e1, env'', store', kon)
    where (env'', store') = updateEnvStore env' store s (Value e1)

-- Accessing a variable reference.
step (Var s, env, store, kon) 
    | addr == Nothing = error $ "Variable " ++ s ++ " is not in the Environment."
    | val == Nothing = error $ "Variable " ++ s ++ " is not in the Store."
    | otherwise = step (fromJust val, env, store, kon)
    where addr = Map.lookup s env
          val = Map.lookup (fromJust addr) store

-- Equality binary operation.
step (Op (CompOp Equality e1 e2), env, store, kon) = step (e1, env, store, (HBinOp $ BinCompOp Equality e2):kon)

step (Value e1, env, store, (HBinOp (BinCompOp Equality e2)):kon) = step (e2, env, store, (BinOpH $ BinCompOp Equality $ Value e1):kon)

step (Value (VBool b'), env, store, (BinOpH (BinCompOp Equality (Value (VBool b)))):kon) = step (Value $ VBool $ b == b', env, store, kon)
step (Value (VInt n'), env, store, (BinOpH (BinCompOp Equality (Value (VInt n)))):kon) = step (Value $ VBool $ n == n', env, store, kon)
step (Value e1, env, store, (BinOpH (BinCompOp Equality (Value e2))):kon) = error $ "Type Error: '==' operation must be between the same types, in " ++ (show e1) ++ " == " ++ (show e2)

-- Cons binary operation.
step (Op (Cons e1 e2), env, store, kon) = step (e1, env, store, (HBinOp $ BinConsOp e2 env):kon)
step (Value e1, env, store, (HBinOp (BinConsOp e2 env')):kon) = step (e2, env', store, (BinOpH (BinConsOp (Value e1) env')):kon)

step (Value (VList []), env, store, (BinOpH (BinConsOp (Value e1) env')):kon) = step (Value $ VList [e1], env', store, kon)

step (Value (VList xs), env, store, (BinOpH (BinConsOp (Value e1) env')):kon)
    | getType (VList (e1:xs)) /= TConflict = step (Value (VList (e1:xs)), env', store, kon)
    | otherwise = error $ "ERRORRRRR type issues plz fix: " ++ (show $ e1) ++ " : " ++ (show $ VList xs)

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

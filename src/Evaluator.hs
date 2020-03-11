module Evaluator where
import Parser
import qualified Data.Map.Strict as Map

-- Environment - A mapping of functions and variables to Closures (which maps an Expression to an Environment).
type Address = Int
data Environment = [ (String, Address) ]
type Store = Map Address ExprType
data ExprType = TInt Int
              | TBool Bool
              --| TFunc [ (Expr, Expr) ]
              deriving (Show)

-- Kontinuation - A stack containing Frames showing what to do.
type Kon = [ Frame ]

-- Frame - Data structures to be put onto the Kontinuation.
data BinOpFrame = BinCompOp ExprComp Expr Env -- Frame for a binary comparison operation - e.g. [-] == e2
                | BinSeqOp Expr -- Frame for a binary sequence operation - e.g. [-] ; e2
                deriving (Show)

data TerOpFrame = TerIfOp Expr (Maybe ExprElif) Env -- Frame for a ternary if statement operation - e.g. if [-] then e1 e2 (e2 is the else/elif)
                deriving (Show)

data Frame = HBinOp BinOpFrame
           | BinOpH BinOpFrame    
           | HTerOp TerOpFrame
           | TerOpH TerOpFrame
           | Done 
           deriving (Show)

-- State - The current state/configuration of the CESK machine.
type State = (Expr, Environment, Store, Kon)

-- Evaluation function to take an Expression (Control) and run it on the finite state machine.
eval :: Expr -> State
eval e = step (e, Env (empty) Nothing, [Done])

-- Step function to move from one State to another.
step :: State -> State

step (Seq e1 e2, env, store, kon) = step (e1, env, store, (HBinOp $ BinSeqOp e2):kon) -- Not sure If you should store the env for e2 - as it should really get the finished env after e1's been evaluated.

-- Defining a new Var - evaluate e1 before adding the new Var to the Env.
-- DefVar s e1
--      => 1) Evaluate e1 and check its type is okay (e.g. 1:True:3:[] should throw an error)
--      => 2) Check the env and store for the variable:
--          => 2a) if in the env and store, then update, else create a new variable.
step (DefVar s e1, env, store, kon) = 
    | addr == Nothing = step (c, env', updateStore store' addr' c, kon)
    | otherwise = step (c, env, updateStore store' addr c, kon)
    where addr = lookup s env
          (env', addr') = addToEnv env s
          (c,e,store',k) = step (e1, env, store, kon)

step s@(_, _, [Done]) = s

step (exp, env, kon) = error $ "ERROR evaluating expression " ++ (show exp) ++ ", no CESK step defined."

-- Adds a new String to the Environment, and returns a tuple of the new Environment and the created Address.
addToEnv :: Environment -> String -> (Environment, Address)
addToEnv [] x = ([(x,0)], 0)
addToEnv ((s,a):env) x = ((x,a+1) : (s,a) : env, a+1)

-- Updates an Address mapping in the store.
-- If the inputted Address is not in the store, then it will be inserted.
updateStore :: Store -> Address -> Expr -> Store
updateStore store a e1
    | item == Nothing = insert a e1 store
    | otherwise = update (\x -> Just e1) a store
    where item = Map.lookup a store

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
           | DefVar String Env
           | Done 
           deriving (Show)

-- State - The current state/configuration of the CESK machine.
type State = (Expr, Environment, Store, Kon)


-- Evaluation function to take an Expression (Control) and run it on the finite state machine.
eval :: Expr -> State
eval e = step (e, [], empty [Done])


-- Step function to move from one State to another.
step :: State -> State

step (Seq e1 e2, env, store, kon) = step (e1, env, store, (HBinOp $ BinSeqOp e2):kon) -- Not sure If you should store the env for e2 - as it should really get the finished env after e1's been evaluated.

-- Defining a new Var.
-- Looks for the variable in the Env, and replaces it in the Store if it exists, else it creates it.
step (DefVar s e1, env, store, kon) = step (e1, env, store, (DefVar s env):kon)

step (Literal $ EInt n, env, store, (DefVar s env'):kon) = step (End, env'', store', kon)
    where (env'', store') = updatedEnvStore env' store s (TInt n)

step (Literal $ EBool b, env, store, (DefVar s env'):kon) = step (End, env'', store', kon)
    where (env'', store') = updatedEnvStore env' store s (TBool b)

step ()

-- End of evaluation.
step s@(_, _, [Done]) = s

-- No defined step for the current State.
step (exp, env, kon) = error $ "ERROR evaluating expression " ++ (show exp) ++ ", no CESK step defined."


-- Binds a String (variable name) to an expression, updating the environment and store and returning them.
updatedEnvStore :: Environment -> Store -> String -> ExprType -> (Environment, Store)
updatedEnvStore env store s e1 = (env', updateStore store addr (Just $ e1))
    where (env', addr) = case lookup s env of
                                Just (a) -> (env, a)
                                Nothing -> addToEnv env s

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

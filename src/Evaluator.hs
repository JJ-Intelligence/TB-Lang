module Evaluator where
import Parser

-- Environment - A mapping of functions and variables to Closures (which maps an Expression to an Environment).
type Env = [ (String, Closure) ]
data Closure = Clo (E, Env) deriving (Eq, Show)

-- Kontinuation - A stack containing Frames showing what to do.
type Kon = [ Frame ]
data Frame = Done deriving (Show)

-- State - The current state/configuration of a CEK machine.
type State = (Expr, Env, Kon)

-- Evaluation function to take an Expression (Control) and run it on the finite state machine.
eval :: Expr -> State
eval exp = step (exp, env, kon)

-- Step function to move from one State to another.
step :: State -> State


step s@(_, _, [Done]) = s

step (exp, env, kon) = error $ "ERROR evaluating expression " ++ (show exp) 
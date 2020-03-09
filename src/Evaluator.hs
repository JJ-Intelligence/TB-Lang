module Evaluator where
import Parser

-- Environment - A mapping of functions and variables to Closures (which maps an Expression to an Environment).
type Env = [ (String, Closure) ]
data Closure = Clo (Expr, Env) deriving (Show)

-- Kontinuation - A stack containing Frames showing what to do.
type Kon = [ Frame ]

-- Frame - Data structures to be put onto the Kontinuation.
data BinOpFrame = BinCompOp ExprComp Expr Env -- Frame for a binary comparison operation - e.g. [-] == e2
                | BinSeqOp Expr Env -- Frame for a binary sequence operation - e.g. [-] ; e2
                deriving (Show)

data TerOpFrame = TerIfOp Expr (Maybe ExprElif) Env -- Frame for a ternary if statement operation - e.g. if [-] then e1 e2 (e2 is the else/elif)
                deriving (Show)

data Frame = HBinOp BinOpFrame
           | BinOpH BinOpFrame
           | HTerOp TerOpFrame
           | TerOpH TerOpFrame
           | Done 
           deriving (Show)

-- State - The current state/configuration of the CEK machine.
type State = (Expr, Env, Kon)

-- Evaluation function to take an Expression (Control) and run it on the finite state machine.
eval :: Expr -> State
eval e = step (e, [], [Done])

-- Step function to move from one State to another.
step :: State -> State


step s@(_, _, [Done]) = s

step (exp, env, kon) = error $ "ERROR evaluating expression " ++ (show exp) 
module Preprocessor where
import Expression
import qualified Data.Map.Strict as Map
import Data.Maybe
-- import Data.List
import Debug.Trace

type TStore = Map String [Type]

type ProcessState = (Expr, Type, TStore, TStore)

beginProcessing :: Expr -> Expr

process :: ProcessState -> ProcessState
process (Seq e1 e2, t, global, local) = process (e2, t', global', local')
    where (_, t', global', local') = process (e1, global, local)

process (Literal (EInt n), t, global, local) = (Value $ VInt n, TInt, global, local)
process (Literal (EBool b), t, global, local) = (Value $ VBool b, TBool, global, local)
process (Literal Empty, t, global, local) = (Value $ VList (TList TEmpty) [], TList TEmpty, global, local)
process (Literal ENone, t, global, local) = (Value VNone, TNone, global, local)

process (LocalAssign (DefVar s (FuncType ps out cs)), )
process (LocalAssign (DefVar s (Func ps e1)), )

process (LocalAssign (DefVar s e1), t, global, local) = (Value v, t', global', Map.insert s t' local')
    where 
        (Value v, t', global', local') = process (e1, t, global, local)






process (e, global, local)
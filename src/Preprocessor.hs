module Preprocessor where
import Expression
import qualified Data.Map.Strict as Map
import Data.Maybe
-- import Data.List
import Debug.Trace
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Evaluator

type TStore = Map.Map String [Type]

type ProcessState = (TStore, TStore, [Type])

preprocess :: Expr -> IO ()
preprocess e = do
    (t, (global, local, ft)) <- process e (Map.empty, Map.empty, [])

    if (ft /= []) then do
        printStdErr ("ERROR: Unable to return when not in a function: plz guess where the error is")
        exitFailure
    else return ()

    return ()

process :: Expr -> ProcessState -> IO ([Type], ProcessState)
process (Seq e1 (Return e2)) (global, local, ft) = do
    (t', (global', local', ft')) <- process e1 (global, local, ft)
    process (Return e2) (global', local', init ft')

process (Seq e1 e2) (global, local, ft) = do
    (t', (global', local', ft')) <- process e1 (global, local, ft)
    process e2 (global', local', ft')

process (Literal (EInt n)) (global, local, ft) = return ([TInt], (global, local, ft))
process (Literal (EBool b)) (global, local, ft) = return ([TBool], (global, local, ft))
process (Literal Empty) (global, local, ft) = return ([TList TEmpty], (global, local, ft))
process (Literal ENone) (global, local, ft) = return ([TNone], (global, local, ft))

--process (LocalAssign (DefVar s (FuncType ps out cs)), )
--process (LocalAssign (DefVar s (Func ps e1)), )

process (LocalAssign (DefVar s e1)) (global, local, ft) = do
    (t', (global', local', ft')) <- process e1 (global, local, ft)
    let lv = (lookupT s (global, local))
    if lv /= Nothing && (length (fromJust lv) /= 1 || (fromJust lv) /= t')
        then printStdErr ("WARNING: ambiguous types for: "++(show s) ++ " in " ++ (show (LocalAssign (DefVar s e1))))
        else return ()

    return (t', (global', Map.insert s t' local', ft'))

process (Var s) state@(global, local, ft) = do
    let t = (lookupT s (global, local))
    if t == Nothing
        then do
            printStdErr ("ERROR: variable referenced before assignment: "++(show s))
            exitFailure
        else return ()
    return ((fromJust t), state)


process (Op (Cons e1 e2)) (global, local, ft) = do
    (t1, (global1, local1, ft1)) <- process e1 (global, local, ft)
    (t2, (global2, local2, ft2)) <- process e2 (global1, local1, ft1)
    if (length t1 /= 1 || length t2 /= 1)
        then do
            printStdErr ("ERROR: Ambiguous types for: "++(show (Op (Cons e1 e2)))) -- Conflicting types error
            exitFailure
        else return()

    let t' = consTypes (head t1) (head t2)

    if (t' == TConflict)
        then do
            printStdErr ("ERROR: conflicting types for op: "++(show (Op (Cons e1 e2)))) -- Conflicting types error
            exitFailure
        else return()

    return ([t'], (global2, local2, ft2)) -- Is this right?

    where
        consTypes e1 (TList e2)
            | e1 == e2 = TList e1
            | isEmptyList e2 = TList $ buildList e1 e2
            where
                buildList e1 TEmpty = e1
                buildList e1 (TList e2) = TList (buildList e1 e2)

                isEmptyList TEmpty = True
                isEmptyList (TList e1) = isEmptyList e1
                isEmptyList _ = False

        consTypes (TList e1) (TList e2) = TList (consTypes e1 e2)
        consTypes TEmpty e@(TList e2) = e

        consTypes _ _ = TConflict


process (Op (MathOp Plus e1 e2)) (global, local, ft) = do
    (t1, (global1, local1, ft1)) <- process e1 (global, local, ft)
    (t2, (global2, local2, ft2)) <- process e2 (global1, local1, ft1)
    printStdErr ("T1:" ++ (show t1))
    printStdErr ("T2:" ++ (show t2))

    case length t1 == 1 of
        True  -> do
            case head t1 of
                TInt    -> do
                    case head t2 of
                        TInt -> return ([TInt], (global2, local2, ft2))
                        _    -> do
                            printStdErr ("ERROR: conflicting types for op: "++(show (Op (MathOp Plus e1 e2)))) -- Conflicting types error
                            exitFailure

                TList t -> do
                    case head t2 of
                        TList t' -> do
                            case joinListTypes (TList t) (TList t') of
                                TConflict -> do
                                    printStdErr ("ERROR: conflicting types for op: "++(show (Op (MathOp Plus e1 e2)))) -- Conflicting types error
                                    exitFailure

                                newT -> return([newT], (global2, local2, ft2))

                            where
                                joinListTypes TInt TInt = TInt
                                joinListTypes TBool TBool = TBool
                                joinListTypes t TEmpty = t
                                joinListTypes TEmpty t = t
                                joinListTypes TNone TNone = TNone
                                joinListTypes (TRef t) (TRef t')
                                    | newT /= TConflict = TRef newT
                                    | otherwise = TConflict
                                    where
                                        newT = joinListTypes t t'

                                joinListTypes (TList t) (TList t')
                                    | newT /= TConflict = TList newT
                                    | otherwise = TConflict
                                    where
                                        newT = joinListTypes t t'

                                joinListTypes _ _ = TConflict

                        _        -> do
                            printStdErr ("ERROR: conflicting types for op: "++(show (Op (MathOp Plus e1 e2)))) -- Conflicting types error
                            exitFailure
                _       -> do
                    printStdErr ("ERROR: conflicting types for op: "++(show (Op (MathOp Plus e1 e2)))) -- Conflicting types error
                    exitFailure

        False -> do
            printStdErr ("ERROR: conflicting types for op: "++(show (Op (MathOp Plus e1 e2)))) -- Conflicting types error
            exitFailure


process (Op (MathOp op e1 e2)) (global, local, ft) = do
    (t1, (global1, local1, ft1)) <- process e1 (global, local, ft)
    (t2, (global2, local2, ft2)) <- process e2 (global1, local1, ft1)

    if length t1 /= 1 || length t2 /= 1
        then do
            printStdErr ("ERROR: ambiguous types for: "++(show (Op (MathOp op e1 e2))))
            exitFailure
        else
            return()

    if t1!!0 /= TInt
        then do
            printStdErr ("ERROR: type invalid for op: "++(show (Op (MathOp op e1 e2))))
            exitFailure
        else return ()

    if t2!!0 /= TInt
        then do
            printStdErr ("ERROR: type invalid for op: "++(show (Op (MathOp op e1 e2))))
            exitFailure
        else return ()

    return ([TInt], (global2, local2, ft2))


process (Op (CompOp op e1 e2)) (global, local, ft) = do
    (t1, (global1, local1, ft1)) <- process e1 (global, local, ft)
    (t2, (global2, local2, ft2)) <- process e2 (global1, local1, ft1)
    if length t1 /= 1 || length t2 /= 1
        then do
            printStdErr ("ERROR: ambiguous types for: "++(show (Op (CompOp op e1 e2))))
            exitFailure
        else
            return()
    case op == And || op == Or of
        True -> do
            if (head t1) /= TBool || (head t2) /= TBool
                then do
                    printStdErr ("ERROR: type invalid for op: "++(show (Op (CompOp op e1 e2))))
                    exitFailure
                else return ()
        False -> do
            case op==Equality of -- TODO add NOT equals and NOT
                True -> do
                    if isChildOf (head t1) CEq && isChildOf (head t2) CEq && (compareTypes [] (head t1) (head t2))
                        then return ()
                        else do
                            printStdErr ("ERROR: type invalid for op: "++(show (Op (CompOp op e1 e2))))
                            exitFailure
                False -> do
                    if isChildOf (head t1) COrd && isChildOf (head t2) COrd && (compareTypes [] (head t1) (head t2))
                        then return ()
                        else do
                            printStdErr ("ERROR: type invalid for op: "++(show (Op (CompOp op e1 e2))))
                            exitFailure
    return ([TBool], (global2, local2, ft2))

process (If c e1 e2) (global, local, ft) = do
    (tc, (global', local', ft')) <- process c (global, local, ft)

    if length tc /= 1 || (head tc) /= TBool then do
        printStdErr ("ERROR: expected boolean: "++(show (If c e1 e2)))
        exitFailure
    else return ()

    (t1, (global_, local_, ft1)) <- process e1 (global', local', ft')
    let (global1, local1) = unionStates (global_, local_) (global', local')

    case e2 of
        Just (Elif c' e1' e2') -> do
            (t2, (global2, local2, ft2)) <- process (If c' e1' e2') (global', local', ft1)
            let (global3, local3) = unionStates (global1, local1) (global2, local2)
            return ([TNone], (global3, local3, ft2))

        Just (Else e) -> do
            (t2, (global2, local2, ft2)) <- process e (global', local', ft1)
            let (global3, local3) = unionStates (global1, local1) (global2, local2)
            return ([TNone], (global3, local3, ft2))

        _ -> do
            return ([TNone], (global1, local1, ft1))

process (While c e1) (global, local, ft) = do
    (tc, (global1, local1, ft1)) <- process c (global, local, ft)

    if length tc /= 1 || (head tc) /= TBool then do
        printStdErr ("ERROR: expected boolean in while loop condition: "++(show (While c e1)))
        exitFailure
    else return ()

    (t1, (global2, local2, ft2)) <- process e1 (global1, local1, ft1)
    let (global3, local3) = unionStates (global1, local1) (global2, local2)
    return ([TNone], (global3, local3, ft2))

process (Return e1) (global, local, ft) = do
    (t, (global', local', _)) <- process e1 (global, local, ft)

    if length t /= 1 then do
        printStdErr ("ERROR: ambiguous types for: " ++ (show (Return e1)))
        exitFailure
    else
        return ()

    return ([TNone], (global', local', if (head t `elem` ft) then ft else (head t):ft))

process (FuncBlock e1) (global, local, ft) = do 
    (t, (global', local', ft')) <- process e1 (global, local, [TNone])

    putStrLn ("FuncBlock return types: " ++ (show ft'))

    if length ft' > 1 then do
        printStdErr ("ERROR: ambiguous types for function block: " ++ (show $ FuncBlock e1) ++ "\nPossible types are " ++ (show ft'))
        exitFailure
    else
        return ()

    return ([head ft'], (global', local', ft))

-- Declaring a function type.
process (LocalAssign (DefVar s (FuncType ps out cs))) (global, local, ft) = do
    case (Map.lookup s local) of
        Just (TFunc ps' out' cs')  -> do
            printStdErr ("ERROR: ambiguous types for function \'"++ s ++ "\'.\nIts type is already: " ++ (show (TFunc ps' out' cs')) 
                ++ "\nBut a second type definition has also been give: " ++ (show (FuncType ps out cs)))
            exitFailure

        _ -> do
            let ts = evaluateFuncType (FuncType ps out cs)
            return ([ts], (global, Map.insert s ts local, ft))

-- Declaring a function definition.
process (LocalAssign (DefVar s (Func ps' e1))) (global, local, ft) = do
    case (lookupT s (global, local)) of
        Nothing -> do
            printStdErr ("ERROR: function type for \'" ++ s ++ "\' has not been declared.")
            exitFailure

        Just (TFunc ps out cs) -> do
            local <- putFuncParamsInStore cs ps ps' local
            (t, (global', local', _)) <- process e1 (global, local, [])

            if length t /= 1 then do
                printStdErr ("ERROR: ambiguous types for: " ++ (show (Return e1)))
                exitFailure
            else
                return ()

            if (head t) /= out then do
                printStdErr ("ERROR: function definition for \'" ++ s ++ "\' doesn't match the functions return type.\nThe function return type is: " ++ (show out) ++ "\nBut the function definition return type is: " ++ (show $ head t))
                exitFailure
            else
                return ()

            return ([(TFunc ps out cs)], global', local, ft)

        _ -> do
            printStdErr ("ERROR: \'" ++ s ++ "\' is not a function.")
            exitFailure


process e s = return ([TNone], s)

--process a b = return ([VNone], b)

-- [Type] is the list of return types of this func block - should return a singleton at the end, else there's a type conflict.
--processFuncBlock :: Expr -> ProcessState -> [Type] -> IO ([Type], ProcessState, [Type])


putFuncParamsInStore :: [(String, TypeClass)] -> [Type] -> Parameters -> TStore -> IO (TStore)
putFuncParamsInStore cs ts ps local
    where

        --Input: [[Int]] (Cons e1 e2) -> [(s, [Int]), ...]
        matchToType :: Type -> Expr -> [(String, Type)]
        matchToType ts (Cons e1 (Cons e2 e3))
        




unionStates :: (TStore, TStore) -> (TStore, TStore) -> (TStore, TStore)
unionStates (global1, local1) (global2, local2) = (Map.unionWith (unionLists) global1 global2, Map.unionWith (unionLists) local1 local2)
   where
       unionLists [] ys = ys
       unionLists (x:xs) (ys)
           | x `elem` ys = unionLists xs ys
           | otherwise = unionLists xs (x:ys)

mergeTList :: [Type] -> [Type] -> [Type]
mergeTList t1 t2 = [x | x <- t1, not (x `elem` t2)] ++ t2

lookupT :: String -> (TStore, TStore) -> Maybe [Type]
lookupT name (global, local)
    | lLookup /= Nothing = lLookup
    | gLookup /= Nothing = gLookup
    | otherwise = Nothing
    where
        lLookup = Map.lookup name local
        gLookup = Map.lookup name global

printStdErr :: String -> IO ()
printStdErr s = do
    hPutStrLn stderr s
    return ()




--process (Seq e1 e2, t, global, local) =

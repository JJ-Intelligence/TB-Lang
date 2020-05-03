-- The Evaluator evaluates the types of expression in an AST, throwing any type errors which arise.
module Preprocessor where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set (fromList, toList)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Data.Maybe (fromJust)

import Expression

type TStore = Map.Map String [Type]
type TypeConstraints = [(String, TypeClass)]
type ProcessState = (TStore, TStore, ([Type], TypeConstraints))

-- Insert inbuilt functions/variables into the store.
insertBuiltIn :: TStore -> TStore
insertBuiltIn local = foldr (\(s,t,_) acc -> Map.insert s [t] acc) local getInBuiltVars

-- Starts up the process function by passing in an AST.
preprocess :: Expr -> IO ()
preprocess e = do
    (t, (global, local, (ft, _))) <- process e (Map.empty, insertBuiltIn (Map.empty), ([],[]))

    if (ft /= []) then do
        printStdErr ("Return ERROR: Unable to return when not inside a function.")
        exitFailure
    else return ()

    return ()

-- Takes in an expression and the current ProcessState.
-- Returns a list of types which the expression could evaluate to, and an updated ProcessState.
process :: Expr -> ProcessState -> IO ([Type], ProcessState)
process (Seq e1 (Return e2 l)) (global, local, (ft,tc)) = do
    (t', (global', local', (ft', tc'))) <- process e1 (global, local, (ft, tc))
    (t'', (global'', local'', (ft'', tc''))) <- process (Return e2 l) (global', local', (ft', tc'))    
    return (t'', (global'', local'', (if length ft'' > 1 then init ft'' else ft'', tc'')))

process (Seq e1 e2) (global, local, ft) = do
    (t', (global', local', ft')) <- process e1 (global, local, ft)
    process e2 (global', local', ft')

process (Literal (EInt n)) (global, local, ft) = return ([TInt], (global, local, ft))
process (Literal (EBool b)) (global, local, ft) = return ([TBool], (global, local, ft))
process (Literal Empty) (global, local, ft) = return ([TList TEmpty], (global, local, ft))
process (Literal ENone) (global, local, ft) = return ([TNone], (global, local, ft))

process (BooleanNotExpr e1 l) (global, local, ft) = do
    (t, (global', local', _)) <- process e1 (global, local, ft)

    if length t /= 1 then do
            printStdErr ("Type ERROR: In statement \'" ++ (show (BooleanNotExpr e1 l)) ++ "\'" ++ (printPos l)
                ++ "\nExpression e1 \'" ++ (show e1) ++ "\' has ambiguous types: " ++ (show $ head t) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t))
            exitFailure
    else
        return ()

    if head t /= TBool then do
            printStdErr ("Type ERROR: In statement \'" ++ (show (BooleanNotExpr e1 l)) ++ "\'" ++ (printPos l)
                ++ "\nExpression e1 \'" ++ (show e1) ++ "\' has type: " ++ (show $ head t)
                ++ "\nBut it should have type Boolean")
            exitFailure
    else
        return ()

    return ([TBool], (global', local', ft))

process (AddressExpr e1 l) (global, local, ft) = do
    (t, (global', local', _)) <- process e1 (global, local, ft)

    if length t /= 1 then do
            printStdErr ("Type ERROR: In statement \'" ++ (show (AddressExpr e1 l)) ++ "\'" ++ (printPos l)
                ++ "\nExpression e1 \'" ++ (show e1) ++ "\' has ambiguous types: " ++ (show $ head t) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t))
            exitFailure
    else
        return ()

    return ([TRef $ head t], (global', local', ft))


process (Return e1 l) (global, local, (ft, tc)) = do
    (t, (global', local', _)) <- process e1 (global, local, (ft,tc))

    if length t /= 1 then do
            printStdErr ("Type ERROR: In statement \'" ++ (show (Return e1 l)) ++ "\'" ++ (printPos l)
                ++ "\nExpression e1 \'" ++ (show e1) ++ "\' has ambiguous types: " ++ (show $ head t) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t))
            exitFailure
    else
        return ()

    return ([TNone], (global', local', ((if (head t `elem` ft) then ft else (head t):ft), tc)))


process (FuncBlock e1 l) (global, local, (ft, tc)) = do
    (t, (global', local', (ft', tc'))) <- process e1 (global, local, ([TNone], tc))
    case isReturn e1 && length ft' == 2 of
        True -> return (filter (/= TNone) ft', (global', local', (ft,tc)))
        False -> do
            if length ft' /= 1 then do
                printStdErr ("Type ERROR: In function block \'" ++ (show (FuncBlock e1 l)) ++ "\'" ++ (printPos l)
                    ++ "\nReturn types are ambiguous: " ++ (show $ head ft') ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail ft'))
                exitFailure
            else
                return ()
            return (ft', (global', local', (ft,tc)))

    where
        isReturn (Return _ _) = True
        isReturn _ = False


-- Declaring a function type.
process (LocalAssign (DefVar s (ExprType (FuncType ps out cs)) l)) (global, local, ft) = do
    case (Map.lookup s local) of
        Just ([TFunc ps' out' cs'])  -> do
            printStdErr ("Type ERROR: In function type definition \'" ++ (show $ LocalAssign (DefVar s (ExprType (FuncType ps out cs)) l)) ++ "\'"  ++ (printPos l)
                ++"\nThe functions type has already been declared as: " ++ (show (TFunc ps' out' cs')))
            exitFailure
        _ -> do
            let ts = evaluateFuncType (FuncType ps out cs)
            return ([ts], (global, Map.insert s [ts] local, ft))


-- Declaring a function definition.
process (LocalAssign (DefVar s (Func ps' e1) l)) (global, local, (ft, tc)) = do
    case (lookupT s (global, local)) of
        Nothing -> do
            printStdErr ("Type ERROR: In function definition \'" ++ (show (LocalAssign (DefVar s (Func ps' e1) l))) ++ "\'" ++ (printPos l)
                ++"\nThe function type for \'" ++ s ++ "\' has not yet been declared")
            exitFailure

        Just ([TFunc ps out cs]) -> do
            if (hasDuplicateVarNames ps' []) then do
                printStdErr ("Parameter ERROR: In function definition \'" ++ (show (LocalAssign (DefVar s (Func ps' e1) l))) ++ "\'" ++ (printPos l)
                    ++ "\nDuplicate variable names have been used")
                exitFailure
            else
                return ()

            case (putFuncParamsInStore cs ps ps' (Map.empty) []) of
                (Left (es, ts)) -> do
                    printStdErr ("Type ERROR: In function definition \'" ++ (show (LocalAssign (DefVar s (Func ps' e1) l))) ++ "\'" ++ (printPos l)
                        ++ "\nThe function definition's parameters don't match the functions type:"
                        ++ "\nThe functions type is: " ++ (show $ TFunc ps out cs) ++ "\n"
                        ++ (foldr (\(t,e) acc -> "Input type \'" ++ (show t) ++ "\' does not match the type of expression \'" ++ (show e) ++ "\'\n"++acc) "" es)
                        ++ "\n" ++ (foldr (\t acc -> "Input type \'" ++ (show t) ++ "\' is not matched in the function definition\n"++acc) "" ts))
                    exitFailure

                (Right funcLocal) -> do
                    let funcGlobal = if ft == [] then local else global

                    (t, (funcGlobal', funcLocal', _)) <- process e1 (funcGlobal, funcLocal, ([], cs))

                    if length t /= 1 then do
                        printStdErr ("Type ERROR: In function definition \'" ++ (show (LocalAssign (DefVar s (Func ps' e1) l))) ++ "\'" ++ (printPos l)
                            ++ "\nFunction definition has ambiguous return types: " ++ (show $ head t) ++ (foldr (\t' acc -> ", " ++ (show t') ++ acc) "" (tail t))
                            ++ "\nBut the return type should be: " ++ (show out) ++ " ~ " ++ (show cs))
                        exitFailure
                    else
                        return ()

                    if not (compareTypes cs out (head t)) then do
                        printStdErr ("Type ERROR: In function definition \'" ++ (show (LocalAssign (DefVar s (Func ps' e1) l))) ++ "\'" ++ (printPos l)
                            ++ "\nFunction definition has a return type of:" ++ (show out) ++ " ~ " ++ (show cs)
                            ++ "\nBut the return type should be: " ++ (show $ head t) ++ " ~ " ++ (show cs))
                        exitFailure
                    else
                        return ()

                    return $ if ft == [] 
                        then ([(TFunc ps out cs)], (global, funcGlobal', (ft, tc))) 
                        else ([(TFunc ps out cs)], (funcGlobal', local, (ft, tc)))

        _ -> do
            printStdErr ("Access ERROR: In function definition \'" ++ (show (LocalAssign (DefVar s (Func ps' e1) l))) ++ "\'" ++ (printPos l)
                ++ "\n" ++ s ++ " is a variable, not a function")
            exitFailure

    where 
        hasDuplicateVarNames FuncParamEnd ls = helper ls 
            where
                helper [] = False
                helper (s:ls) = s `elem` ls || helper ls

        hasDuplicateVarNames (FuncParam e1 e2) ls = hasDuplicateVarNames e2 $ helper e1 ls
            where 
                helper (Var s _) ls = s:ls
                helper (PointerExpr e1) ls = helper e1 ls
                helper (Op (Cons e1 e2) _) ls = helper e2 $ helper e1 ls
                helper _ ls = ls


process (LocalAssign (DefVar s e1 _)) (global, local, ft) = do
    (t', (global', local', ft')) <- process e1 (global, local, ft)
    return (t', (global', Map.insert s t' local', ft'))

process (GlobalAssign (DefVar s e1 _)) state@(global, local, (ft, _)) = do
    (t', (global', local', ft')) <- process e1 state
    case ft == [] of
        True -> return (t', (global', Map.insert s t' local', ft'))
        False -> return (t', (Map.insert s t' global', local', ft'))

process (DefPointerVar s e1) (global, local, ft) = do
    let t = lookupT s (global, local)
    if t == Nothing then do
        printStdErr ("Access ERROR: In variable access for \'" ++ (show (DefPointerVar s e1)) ++ "\'"
            ++ "\nPointer variable " ++ s ++ " has not been declared yet")
        exitFailure
    else 
        return ()

    if length (fromJust t) /= 1 then do
        printStdErr ("Access ERROR: In variable access for \'" ++ (show (DefPointerVar s e1)) ++ "\'"
            ++ "\nPointer variable has ambiguous types: " ++ (show $ head $ fromJust t) ++ (foldr (\t' acc -> ", " ++ (show t') ++ acc) "" (tail $ fromJust t)))
        exitFailure
    else
        return ()

    if (isNotRef $ head $ fromJust t) then do
        printStdErr ("Access ERROR: In pointer variable access \'" ++ (show (DefPointerVar s e1)) ++ "\'"
            ++ "\nVariable " ++ s ++ " has type "++(show $ head $ fromJust t)++", but it should be a reference")
        exitFailure
    else
        return ()

    (t', (global', local', ft')) <- process e1 (global, local, ft)

    if length (fromJust t) /= 1 then do
        printStdErr ("Access ERROR: In variable assignment for \'" ++ (show (DefPointerVar s e1)) ++ "\'"
            ++ "\nExpression e1 has ambiguous types: " ++ (show $ head t') ++ (foldr (\t' acc -> ", " ++ (show t') ++ acc) "" (tail t')))
        exitFailure
    else
        return ()

    return (t', (global', Map.insert s ([TRef $ head $ t']) local', ft'))

    where
        isNotRef (TRef _) = False
        isNotRef _ = True

process (PointerExpr e1) (global, local, ft) = do
    (t, (global', local', ft')) <- process e1 (global, local, ft)
    if length t /= 1 then do
        printStdErr ("Type ERROR: In pointer expression \'" ++ (show (PointerExpr e1)) ++ "\'"
            ++ "\nPointed expression has ambiguous types: " ++ (show $ head t) ++ (foldr (\t' acc -> ", " ++ (show t') ++ acc) "" (tail t)))
        exitFailure
    else
        return ()

    case head t of
        (TRef t') -> return ([t'], (global', local', ft'))
        t' -> do
            printStdErr ("Type ERROR: In pointer expression \'" ++ (show (PointerExpr e1)) ++ "\'"
                ++ "\nPointed expression \'"++(show e1)++"\' has type " ++ (show t') ++ ", but it should be a reference")
            exitFailure

process (Var s l) state@(global, local, ft) = do
    let t = (lookupT s (global, local))
    if t == Nothing then do
        printStdErr ("Access ERROR: In variable access \'" ++ (show (Var s l)) ++ "\'" ++ (printPos l)
            ++ "\nVariable " ++ s ++ " has not been declared yet")
        exitFailure
    else 
        return ()
    return ((fromJust t), state)

process (GlobalVar s l) state@(global, local, (ft, _)) = do
    let t = lookupGlobal s (if ft == [] then local else global)
    if t == Nothing then do
        printStdErr ("Access ERROR: In variable access \'" ++ (show (Var s l)) ++ "\'" ++ (printPos l)
            ++ "\nGlobal variable " ++ s ++ " has not been declared yet")
        exitFailure
    else 
        return ()
    return ((fromJust t), state)

process (TryCatch e1 ps e2 l) (global, local, ft) = do
    foldr (\e acc -> do
            (t', (global', local', ft')) <- process e (global, local, ft)

            if length t' /= 1 || head t' /= TException then do
                printStdErr ("Type ERROR: In try-catch block \'"++(show $ TryCatch e1 ps e2 l)++"\'" ++ (printPos l)
                    ++ "\nIn catch block parameters, value \'" ++ (show e) ++ "\'"
                    ++ (if length t' /= 1 then " has ambiguous types: " ++ (show $ head t') ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t') else " has type " ++ (show $ head t'))
                    ++ "\nBut all catch block parameters should have type Exception")
                exitFailure
            else
                return ()

            acc' <- acc
            return (acc'+1)
        ) (return 0) (paramsToList ps)

    (t', (global', local', ft')) <- process e1 (global, local, ft)
    (t', (global', local', ft')) <- process e2 (global, local, ft)
    return ([TNone], (global', local', ft'))

    where
        paramsToList :: Parameters -> [Expr]
        paramsToList FuncParamEnd = []
        paramsToList (FuncParam e1 e2) = e1 : paramsToList e2 


process (FuncCall s ps l) (global, local, (ft, cs)) = do
    case lookupT s (global, local) of
        Nothing -> do
            printStdErr ("Access ERROR: In function call \'"++(show $ FuncCall s ps l)++"\'" ++ (printPos l)
                ++ "\nFunction " ++ s ++ " has not been declared")
            exitFailure

        Just ([TFunc ps' out ts]) -> do
            (ms, (global', local')) <- matchParamsToType ts (global, local, cs) ps' ps []

            if ms /= Nothing then do
                printStdErr ("Type ERROR: In function call \'"++(show $ FuncCall s ps l)++"\'" ++ (printPos l)
                    ++ "\nDeclared input types don't match the types of the function calls parameters:\n"
                    ++ (foldr (\(t, (e, t')) acc -> "Type \'" ++ (show t) ++ (if ts == [] then "" else " ~ ("++(showTypeConstraints ts)++")") ++ "\' doesn't match the type of parameter \'"++(show e)++"\' ("++(show t')++")"++acc) "" (fromJust ms)))
                exitFailure
            else
                return ()

            let g = containsGeneric out
            case g == TConflict of
                True -> return ([out], (global', local', (ft, cs)))
                False -> do
                    let (TGeneric g') = g 
                    let xs = foldr (\(t,n) acc -> if isGenericInFuncParam g' t then (t,n):acc else acc) [] (zip ps' [0..]) -- xs = [(Type, Int)] (function parameter types)

                    case length xs < 1 of
                        True -> return ([out], (global', local', (ft, cs)))

                        False -> do
                            ys <- foldr (\e acc -> do
                                            (t', (global', local', ft')) <- process e (global, local, ([], cs))
                                            acc' <- acc
                                            return (head t':acc')) (return []) (foldOverParams (map snd xs) 0 ps) -- ys = IO [(Type, Int)] (function argument types)

                            zs <- getGenericTypes g' (map fst xs) ys
                            
                            case (length (Set.fromList zs)) == 1 of
                                False -> do
                                    let ts = Set.toList (Set.fromList zs)
                                    printStdErr ("Type ERROR: In function call \'"++(show $ FuncCall s ps l)++"\'" ++ (printPos l)
                                        ++"\nMultiple types are provided for generic \'" ++ g' ++ "\' in the function calls parameters: " ++ (show $ head ts) ++ (foldr (\t acc -> ", " ++ (show t) ++ acc) "" (tail ts)))
                                    exitFailure

                                True -> do
                                    let rt = subTypeIntoGeneric g' (head zs) out

                                    if rt == TConflict then do
                                        printStdErr ("Type ERROR: In function call \'"++(show $ FuncCall s ps l)++"\'" ++ (printPos l)
                                            ++ "\nUnable to infer the generic return type of the function")
                                        exitFailure
                                    else
                                        return ()

                                    return ([rt], (global', local', (ft, cs)))

        Just _ -> do
            printStdErr ("Access ERROR: In function call \'" ++ (show (FuncCall s ps l)) ++ "\'"  ++ (printPos l)
                ++ "\n" ++ s ++ " is a variable, not a function")
            exitFailure

    where
        matchParamsToType :: TypeConstraints -> (TStore, TStore, TypeConstraints) -> [Type] -> Parameters -> [(Type, (Expr, Type))] -> IO (Maybe [(Type, (Expr, Type))], (TStore, TStore))
        matchParamsToType tc (global, local, cs) ts (FuncParamEnd) es
            | ts /= [] || es /= [] = return (Just $ es, (global, local))
            | otherwise = return (Nothing, (global, local))
        matchParamsToType tc (global, local, cs) [] _ es = do
            printStdErr ("Type ERROR: In function call \'" ++ (show (FuncCall s ps l)) ++ "\'" ++ (printPos l)
                ++ "\nThere are more arguments given, than parameters declared for this function.") 
            exitFailure
        matchParamsToType tc (global, local, cs) (t:ts) (FuncParam e1 e2) es = do
            (t', (global', local', ft')) <- process e1 (global, local, ([], cs))

            if length t' /= 1 then do
                printStdErr ("Type ERROR: In function call \'" ++ (show (FuncCall s ps l)) ++ "\'" ++ (printPos l)
                    ++ "\nParameter \'" ++ (show e1) ++  "\' has ambiguous types: " ++ (show $ head t') ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t')
                    ++ "\nIts type should be: " ++ (show t) ++ " ~ " ++ (show tc))
                exitFailure
            else
                return ()

            if (compareTypes tc t (head t'))
                then matchParamsToType tc (global', local', cs) ts e2 es
                else matchParamsToType tc (global, local, cs) ts e2 ((t, (e1, head t')):es)

        -- Takes in a Type.
        -- Returns TConflict if the type contains no generics, else returns the generic.
        containsGeneric :: Type -> Type
        containsGeneric (TList t) = containsGeneric t
        containsGeneric (TRef t) = containsGeneric t
        containsGeneric (TIterable t) = containsGeneric t
        containsGeneric g@(TGeneric s) = g
        containsGeneric _ = TConflict

        -- Takes in the name of a generic, and a parameter type.
        -- Returns a False if the generic is not found in the type, else returns True.
        isGenericInFuncParam :: String -> Type -> Bool
        isGenericInFuncParam s (TList t) = isGenericInFuncParam s t
        isGenericInFuncParam s (TRef t) = isGenericInFuncParam s t
        isGenericInFuncParam s (TIterable t) = isGenericInFuncParam s t
        isGenericInFuncParam s (TGeneric s') = s == s'
        isGenericInFuncParam s (TFunc ps out ts) = isInFuncPs s ps || isGenericInFuncParam s out
            where
                isInFuncPs s [] = False
                isInFuncPs s (p:ps) = isGenericInFuncParam s p || isInFuncPs s ps
        isGenericInFuncParam _ _ = False

        foldOverParams :: [Int] -> Int -> Parameters -> [Expr]
        foldOverParams [] i ps = []
        foldOverParams (n:ns) i (FuncParam e1 e2)
            | i == n = e1 : foldOverParams ns (i+1) e2
            | otherwise = foldOverParams (n:ns) (i+1) e2

        -- Takes in the unkown generic name, a list of parameter Types (all of which contain generics), and a list of their corresponding 
        -- arguments in the function call.
        -- Returns a list of the generics actual types, inferred from the function call arguments.
        getGenericTypes :: String -> [Type] -> [Type] -> IO [Type]
        getGenericTypes g [] _ = return []
        getGenericTypes g (t:tp) (t':ta) = do
            case getGenType g t t' of
                [TConflict] -> do
                    printStdErr ("Type ERROR: In function call \'" ++ (show (FuncCall s ps l)) ++ "\'" ++ (printPos l)
                        ++ "\nA parameter has type: " ++ (show t')
                        ++ "\nBut its type should be: " ++ (show t))
                    exitFailure
                t -> do
                    ts <- getGenericTypes g tp ta
                    return (t++ts)
            where
                -- Takes in the unknown generic name, a parameter Type containing a generic, and its corresponding argument Type 
                -- from the function call.
                -- Returns a list of Types which the generic' in the parameter type will map to.
                getGenType :: String -> Type -> Type -> [Type]
                getGenType g (TList t) (TList t') = getGenType g t t'
                getGenType g (TRef t) (TRef t') = getGenType g t t'
                getGenType g (TIterable t) (TList t') = getGenType g t t'
                getGenType g (TIterable (TGeneric a)) (TStream) = [TInt]
                getGenType g (TGeneric a) t
                    | a == g = [t]
                    | otherwise = [TConflict]
                getGenType g (TFunc ps out ts) (TFunc ps' out' ts')
                    | tout == [TConflict] && tps == [] = [TConflict]
                    | tout == [TConflict] = tps
                    | otherwise = tps ++ tout
                    where
                        tout = getGenType g out out'
                        tps = foldr (\(p,p') acc -> let t = (getGenType g p p') in 
                                     if t == [TConflict] then acc else t ++ acc) [] (zip ps ps')

                getGenType _ _ _ = [TConflict]
        subTypeIntoGeneric :: String -> Type -> Type -> Type
        subTypeIntoGeneric g t (TList t')
            | rt == TConflict = TConflict
            | otherwise = TList rt
            where rt = subTypeIntoGeneric g t t'
        subTypeIntoGeneric g t (TRef t')
            | rt == TConflict = TConflict
            | otherwise = TRef rt
            where rt = subTypeIntoGeneric g t t'
        subTypeIntoGeneric g t (TIterable t')
            | rt == TConflict = TConflict
            | otherwise = TIterable rt
            where rt = subTypeIntoGeneric g t t'
        subTypeIntoGeneric g t (TGeneric a) = t
        subTypeIntoGeneric _ _ _ = TConflict

        showTypeConstraints :: [(String, TypeClass)] -> String
        showTypeConstraints [] = ""
        showTypeConstraints [(s,c)] = (show c)++" "++s
        showTypeConstraints ((s,c):tc) = (show c)++" "++s++", "++(showTypeConstraints tc)


process (Op (Cons e1 e2) l) (global, local, ft) = do
    (t1, (global1, local1, ft1)) <- process e1 (global, local, ft)
    (t2, (global2, local2, ft2)) <- process e2 (global1, local1, ft1)

    if length t1 /= 1 || length t2 /= 1 then do
        printStdErr ("Type ERROR: In operation \'" ++ (show (Op (Cons e1 e2) l)) ++ "\'" ++ (printPos l)
            ++ (if length t1 /= 1 then "\nExpression e1 \'" ++ (show e1) ++ "\' has ambiguous types: " ++ (show $ head t1) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t1) else "")
            ++ (if length t2 /= 1 then "\nExpression e2 \'" ++ (show e2) ++ "\' has ambiguous types: " ++ (show $ head t2) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t2) else ""))
        exitFailure
    else
        return()


    let t' = consTypes (head t1) (head t2)

    if (t' == TConflict) then do
        printStdErr ("Type ERROR: In operation \'" ++ (show (Op (Cons e1 e2) l)) ++ "\'" ++ (printPos l)
            ++ "\nExpression e1 \'" ++ (show e1) ++ "\' has type: " ++ (show $ head t1)
            ++ "\nExpression e2 \'" ++ (show e2) ++ "\' has type: " ++ (show $ head t2)
            ++ (if isList $ head t2 then let (TList t) = head t2 in "\nBut e1 should have type " ++ (show t) else "\nBut e2 should have type List"))
        exitFailure
    else 
        return()

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

        isList (TList _) = True
        isList _ = False


process (Op (MathOp Plus e1 e2) l) (global, local, ft) = do
    (t1, (global1, local1, ft1)) <- process e1 (global, local, ft)
    (t2, (global2, local2, ft2)) <- process e2 (global1, local1, ft1)

    case length t1 == 1 && length t2 == 1 of
        True  -> do
            case (head t1, head t2) of
                (TInt, TInt) -> return ([TInt], (global2, local2, ft2))

                (TList t, TList t') -> do
                    case joinListTypes (TList t) (TList t') of
                        TConflict -> do
                            printStdErr ("Type ERROR: In operation \'" ++ (show (Op (MathOp Plus e1 e2) l)) ++ "\'" ++ (printPos l)
                                ++ "\nExpression e1 \'" ++ (show e1) ++ "\' has type: " ++ (show $ TList t)
                                ++ "\nExpression e2 \'" ++ (show e2) ++ "\' has type: " ++ (show $ TList t')
                                ++ "\nBut both expressions should have the same List types")
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

                (k1, k2) -> do
                    printStdErr ("Type ERROR: In operation \'" ++ (show (Op (MathOp Plus e1 e2) l)) ++ "\'" ++ (printPos l)
                        ++ "\nExpression e1 \'" ++ (show e1) ++ "\' has type: " ++ (show k1)
                        ++ "\nExpression e2 \'" ++ (show e2) ++ "\' has type: " ++ (show k2)
                        ++ "\nBut both expressions should have either Int types, or List types")
                    exitFailure

        False -> do -- Conflicting types error (e1)
            printStdErr ("Type ERROR: In operation \'" ++ (show (Op (MathOp Plus e1 e2) l)) ++ "\'" ++ (printPos l)
                ++ (if length t1 /= 1 then "\nExpression e1 \'" ++ (show e1) ++ "\' has ambiguous types: " ++ (show $ head t1) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t1) else "")
                ++ (if length t2 /= 1 then "\nExpression e2 \'" ++ (show e2) ++ "\' has ambiguous types: " ++ (show $ head t2) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t2) else "")
                ++ "\nBut expressions should have type Int or List")
            exitFailure



process (Op (MathOp op e1 e2) l) (global, local, ft) = do
    (t1, (global1, local1, ft1)) <- process e1 (global, local, ft)
    (t2, (global2, local2, ft2)) <- process e2 (global1, local1, ft1)

    if length t1 /= 1 || length t2 /= 1 then do
        printStdErr ("Type ERROR: In operation \'" ++ (show (Op (MathOp op e1 e2) l)) ++ "\'" ++ (printPos l)
            ++ (if length t1 /= 1 then "\nExpression e1 \'" ++ (show e1) ++ "\' has ambiguous types: " ++ (show $ head t1) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t1) else "")
            ++ (if length t2 /= 1 then "\nExpression e2 \'" ++ (show e2) ++ "\' has ambiguous types: " ++ (show $ head t2) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t2) else "")
            ++ "\nBut expressions should have type Int")
        exitFailure
    else
        return()

    if head t1 /= TInt || head t2 /= TInt then do
        printStdErr ("Type ERROR: In operation \'" ++ (show (Op (MathOp op e1 e2) l)) ++ "\'" ++ (printPos l)
            ++ "\nExpression e1 \'" ++ (show e1) ++ "\' has type: " ++ (show $ head t1)
            ++ "\nExpression e2 \'" ++ (show e2) ++ "\' has type: " ++ (show $ head t2)
            ++ "\nBut both expressions should have Int types")
        exitFailure
    else
        return ()

    return ([TInt], (global2, local2, ft2))


process (Op (CompOp op e1 e2) l) (global, local, (ft, tc)) = do
    (t1, (global1, local1, ft1)) <- process e1 (global, local, (ft, tc))
    (t2, (global2, local2, ft2)) <- process e2 (global1, local1, ft1)

    if length t1 /= 1 || length t2 /= 1 then do
        printStdErr ("Type ERROR: In operation \'" ++ (show (Op (CompOp op e1 e2) l)) ++ "\'" ++ (printPos l)
            ++ (if length t1 /= 1 then "\nExpression e1 \'" ++ (show e1) ++ "\' has ambiguous types: " ++ (show $ head t1) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t1) else "")
            ++ (if length t2 /= 1 then "\nExpression e2 \'" ++ (show e2) ++ "\' has ambiguous types: " ++ (show $ head t2) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail t2) else "")
            ++ "\nBut expressions should have type Int")
        exitFailure
    else
        return()

    case op == And || op == Or of
        True -> do -- &&, || operations
            if (head t1) /= TBool || (head t2) /= TBool then do
                printStdErr ("Type ERROR: In operation \'" ++ (show (Op (CompOp op e1 e2) l)) ++ "\'" ++ (printPos l)
                    ++ "\nExpression e1 \'"++ (show e1) ++"\' has type: " ++ (show $ head t1)
                    ++ "\nExpression e2 \'"++ (show e2) ++"\' has type: " ++ (show $ head t2)
                    ++ "\nBut both expressions should have type Boolean")
                exitFailure
            else 
                return ()

        False -> do
            case op == Equality || op == NotEquals of -- TODO add NOT equals and NOT
                True -> do -- == operation
                    if isChildOf tc (head t1) ([], CEq) && isChildOf tc (head t2) ([], CEq) && compareTypes tc (head t1) (head t2) then 
                        return ()
                    else do
                        printStdErr ("Type ERROR: In operation \'" ++ (show (Op (CompOp op e1 e2) l)) ++ "\'" ++ (printPos l)
                            ++ "\nExpression e1 \'"++ (show e1) ++"\' has type: " ++ (show $ head t1)
                            ++ "\nExpression e2 \'"++ (show e2) ++"\' has type: " ++ (show $ head t2)
                            ++ "\nBut both expressions should be children of the Eq type class")
                        exitFailure

                False -> do -- <, > operations
                    if isChildOf tc (head t1) ([], COrd) && isChildOf tc (head t2) ([], COrd) && compareTypes tc (head t1) (head t2) then 
                        return ()
                    else do
                        printStdErr ("Type ERROR: In operation \'" ++ (show (Op (CompOp op e1 e2) l)) ++ "\'" ++ (printPos l)
                            ++ "\nExpression e1 \'"++ (show e1) ++"\' has type: " ++ (show $ head t1)
                            ++ "\nExpression e2 \'"++ (show e2) ++"\' has type: " ++ (show $ head t2)
                            ++ "\nBut both expressions should be children of the Ord type class")
                        exitFailure

    return ([TBool], (global2, local2, ft2))


process (If c e1 e2 l) (global, local, ft) = do
    (tc, (global', local', ft')) <- process c (global, local, ft)

    if length tc /= 1 || (head tc) /= TBool then do
        printStdErr ("Type ERROR: In statement \'" ++ (show (If c e1 e2 l)) ++ "\'" ++ (printPos l) ++
            (if length tc /= 1 then 
                "\nThe condition \'" ++ (show c) ++ "\' has ambiguous types: " ++ (show $ head tc) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail tc) 
            else 
                "\nThe condition \'"++ (show c) ++ "\' has type: " ++ (show $ head tc))
            ++ "\nBut it should have type Boolean")
        exitFailure
    else 
        return ()

    (t1, (global_, local_, ft1)) <- process e1 (global', local', ft')
    let (global1, local1) = unionStates (global_, local_) (global', local')

    case e2 of
        Just (Elif c' e1' e2' l') -> do
            (t2, (global2, local2, ft2)) <- process (If c' e1' e2' l') (global', local', ft1)
            let (global3, local3) = unionStates (global1, local1) (global2, local2)
            return ([TNone], (global3, local3, ft2))

        Just (Else e) -> do
            (t2, (global2, local2, ft2)) <- process e (global', local', ft1)
            let (global3, local3) = unionStates (global1, local1) (global2, local2)
            return ([TNone], (global3, local3, ft2))

        _ -> do
            return ([TNone], (global1, local1, ft1))

process (While c e1 l) (global, local, ft) = do
    (ts, (global1, local1, ft1)) <- process c (global, local, ft)

    if length ts /= 1 || (head ts) /= TBool then do
        printStdErr ("Type ERROR: In statement \'" ++ (show (While c e1 l)) ++ "\'"  ++ (printPos l) ++
            (if length ts /= 1 then 
                "\nThe condition \'" ++ (show c) ++ "\' has ambiguous types: " ++ (show $ head ts) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail ts) 
            else 
                "\nThe condition \'"++ (show c) ++ "\' has type: " ++ (show $ head ts))
            ++ "\nBut it should have type Boolean")
        exitFailure
    else 
        return ()

    (t1, (global2, local2, ft2)) <- process e1 (global1, local1, ft1)
    -- let (global3, local3) = unionStates (global1, local1) (global2, local2) -- Don't think we need this union?
    return ([TNone], (global2, local2, ft2))

process (For i c n e l) (global, local, ft) = do
    (ts1, (global1, local1, ft1)) <- process i (global, local, ft)
    (ts2, (global2, local2, ft2)) <- process c (global1, local1, ft1)

    if length ts2 /= 1 || head ts2 /= TBool then do
        printStdErr ("Type ERROR: In statement \'" ++ (show (For i c n e l)) ++ "\'" ++ (printPos l) ++
            (if length ts2 /= 1 then 
                "\nThe condition \'" ++ (show c) ++ "\' has ambiguous types: " ++ (show $ head ts2) ++ (foldr (\k acc -> ", " ++ (show k) ++ acc) "" $ tail ts2) 
            else 
                "\nThe condition \'"++ (show c) ++ "\' has type: " ++ (show $ head ts2))
            ++ "\nBut it should have type Boolean")
        exitFailure
    else 
        return ()

    (ts3, (global3, local3, ft3)) <- process n (global2, local2, ft2)
    (ts4, (global4, local4, ft4)) <- process n (global3, local3, ft3)
    return ([TNone], (global4, local4, ft2))

process e s = return ([TNone], s)


-- Returns either an error (a list of correct types to their incorrect parameter expressions), or returns an updated TStore
putFuncParamsInStore :: TypeConstraints -> [Type] -> Parameters -> TStore -> [(Type, Expr)] -> Either ([(Type, Expr)], [Type]) TStore
putFuncParamsInStore tc ts FuncParamEnd local es
    | ts /= [] || es /= [] = Left (es, ts)
    | otherwise = Right local
putFuncParamsInStore tc [] _ _ es = Left (es, [])
putFuncParamsInStore tc (t:ts) (FuncParam e1 e2) local es
    | ms == Nothing = putFuncParamsInStore tc ts e2 local ((t,e1):es)
    | otherwise = putFuncParamsInStore tc ts e2 (foldr (\(s,vt) acc -> Map.insert s [vt] acc) local (fromJust ms)) es
    where
        ms = matchToType tc t e1 []

        --Input: [[Int]] (Cons e1 e2) [] -> [(s, [Int]), ...]
        matchToType :: TypeConstraints -> Type -> Expr -> [(String, Type)] -> Maybe [(String, Type)]
        matchToType tc t (Op (Cons e1 e2) _) ls
            | a /= TConflict = if ls' == Nothing then Nothing else matchToType tc t e2 (fromJust ls')
            | otherwise = Nothing
            where
                a = isItrOrList t
                ls' = matchToType tc a e1 ls

                isItrOrList (TIterable a) = a
                isItrOrList (TList a) = a
                isItrOrList TStream = TInt
                isItrOrList _ = TConflict

        matchToType tc (TRef rt) (PointerExpr e1) ls
            | ls' == Nothing = Nothing
            | length ls'' > 1 = Just $ (tail ls'') ++ [(fst $ last ls'', TRef $ snd $ last ls'')] ++ ls
            | otherwise = Just $ foldr (\(s,t) acc -> (s,TRef t):acc) ls (ls'')
            where 
                ls' = matchToType tc rt e1 []
                ls'' = fromJust ls'

        matchToType tc t (Var s _) ls = Just $ (s, t):ls

        matchToType tc t e ls
            | t' == Nothing || not (compareTypes tc t (fromJust t')) = Nothing
            | otherwise = Just ls
            where 
                t' = getLitType e

        getLitType :: Expr -> Maybe Type
        getLitType (Literal (EInt _)) = Just TInt
        getLitType (Literal (EBool _)) = Just TBool
        getLitType (Literal Empty) = Just $ TList TEmpty
        getLitType (Literal ENone) = Just TNone
        getLitType (ExprType (FuncType ps out cs)) = Just $ evaluateFuncType (FuncType ps out cs)
        getLitType _ = Nothing


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

lookupGlobal :: String -> TStore -> Maybe [Type]
lookupGlobal s global = Map.lookup s global

compareTypes :: [(String, TypeClass)] -> Type -> Type -> Bool
compareTypes tc (TFunc [] out ftc) (TFunc [] out' ftc') = compareTypes tc out out'
compareTypes tc (TFunc (p:ps) out ftc) (TFunc (p':ps') out' ftc') = compareTypes tc p p' && compareTypes tc (TFunc ps out ftc) (TFunc ps' out' ftc')
compareTypes tc TEmpty TEmpty = True

compareTypes tc _ TEmpty = True -- Unsure about this
compareTypes tc (TList e1) (TList TEmpty) = True
compareTypes tc (TList e1) (TList e2) = compareTypes tc e1 e2
compareTypes tc (TList _) (TParamList) = True
compareTypes tc (TRef e1) (TRef e2) = compareTypes tc e1 e2
compareTypes tc (TGeneric s) g@(TGeneric s')
    | s == s' = True
    | c == Nothing = True
    | otherwise = isChildOf tc g (s, fromJust c)
    where
        c = lookup s tc

compareTypes tc (TGeneric s) TParamList = True

compareTypes tc (TGeneric s) e2
    | c == Nothing = if notContainsGeneric s e2 then True else False
    | c == Nothing = False
    | otherwise = isChildOf tc e2 (s, fromJust c)
    where 
        c = lookup s tc

        notContainsGeneric s (TList t) = notContainsGeneric s t
        notContainsGeneric s (TRef t) = notContainsGeneric s t
        notContainsGeneric s (TGeneric s') = s /= s'
        notContainsGeneric _ _ = True

compareTypes tc (TIterable g) (TStream) = compareTypes tc g TInt
compareTypes tc (TIterable g) (TList e2) = compareTypes tc g e2
compareTypes tc (TIterable g) (TParamList) = True
compareTypes tc e1 e2 = e1 == e2

-- Check if a type is a child of a type class.
isChildOf :: [(String, TypeClass)] -> Type -> (String, TypeClass) -> Bool

-- Eq class.
isChildOf tc TInt (s, CEq) = True
isChildOf tc TBool (s, CEq) = True
isChildOf tc TEmpty (s, CEq) = True
isChildOf tc TNone (s, CEq) = True
isChildOf tc TException (s, CEq) = True
isChildOf tc (TList e1) (s, CEq) = isChildOf tc e1 (s, CEq)
isChildOf tc (TGeneric a) (s, CEq)
    | a == s = False -- a == s, so a can't be a child of s
    | t == Nothing = False
    | fromJust t == CEq = True
    | otherwise = False
    where t = lookup a tc
isChildOf _ _ (_, CEq) = False

-- Itr class.
isChildOf tc (TList _) (s, CItr) = True
isChildOf tc TStream (s, CItr) = True
isChildOf tc (TGeneric a) (s, CItr)
    | a == s = False
    | t == Nothing = False
    | fromJust t == CItr = True
    | otherwise = False
    where t = lookup a tc
isChildOf _ _ (s, CItr) = False

-- Ord class.
isChildOf tc TInt (s, COrd) = True
isChildOf tc (TGeneric a) (s, COrd)
    | a == s = False
    | t == Nothing = False
    | fromJust t == COrd = True
    | otherwise = False
    where t = lookup a tc
isChildOf tc _ (s, COrd) = False

-- Print class.
isChildOf tc TInt (s, CPrintable) = True
isChildOf tc (TList TInt) (s, CPrintable) = True
isChildOf tc (TList TEmpty) (s, CPrintable) = True
isChildOf tc _ (s, CPrintable) = False

-- Build a function type.
evaluateFuncType :: Type -> Type 
evaluateFuncType (FuncType ps out cs) = buildTFunc ps out cs
    where
        buildTFunc ps out cs = TFunc (evaluateParams ps) (evaluateOut out) (if cs == Nothing then [] else evaluateConstraints $ fromJust cs)

        evaluateParams FuncParamEnd = []
        evaluateParams (FuncParam (ExprType (FuncType ps' out' cs')) e3) = buildTFunc ps' out' cs' : evaluateParams e3
        evaluateParams (FuncParam (ExprType t) e3) = t : evaluateParams e3

        evaluateOut (ExprType (FuncType ps' out' cs')) = buildTFunc ps' out' cs'
        evaluateOut (ExprType t) = t

        evaluateConstraints FuncParamEnd = []
        evaluateConstraints (FuncParam (TypeConstraint cl g) e3) = (g, cl) : evaluateConstraints e3

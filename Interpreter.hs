module Interpreter where
    import AbsGrammar

    import Data.Map as Map
    import Data.Maybe

    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except

    import Types
    import Memory
    import TypeChecker

-- -- Expressions support

    evalRelOp GTH e1 e2 = e1 > e2
    evalRelOp GE e1 e2 = e1 >= e2
    evalRelOp LTH e1 e2 = e1 < e2
    evalRelOp EQU e1 e2 = e1 == e2
    evalRelOp NE e1 e2 = e1 /= e2
    evalRelOp LE e1 e2 = e1 <= e2

    evalMulOp Div e1 e2 = div e1 e2
    evalMulOp Times e1 e2 = e1 * e2
    evalMulOp Mod e1 e2 = e1 `mod` e2

    evalAddOp Minus e1 e2 = e1 - e2
    evalAddOp Plus e1 e2 = e1 + e2

    evalExpression :: Expr -> RR MemVal

    evalExpression (EVar ident) = readFromMemory ident

    evalExpression (EApp ident vars) = do
        (env2, Just ret) <- runFunc ident vars
        return ret

-- STRUCTS
--    evalExpression (EStructField2 expr field) = do
--        StructVal val <- evalExpression expr
--        extractFieldFromStructHelper val field

-- --  int

    evalExpression (ELitInt x) = return $ IntVal x
    evalExpression (EAdd e1 op e2) = do
        IntVal r1 <- evalExpression e1
        IntVal r2 <- evalExpression e2
        return $ IntVal $ evalAddOp op r1 r2

    evalExpression (EMul e1 op e2) = do
        IntVal r1 <- evalExpression e1
        IntVal r2 <- evalExpression e2
        case op of
            Div -> if r2 == 0 then throwError DivisionByZeroException else return $ IntVal $ evalMulOp op r1 r2
            Mod -> if r2 == 0 then throwError ModulusByZeroException else return $ IntVal $ evalMulOp op r1 r2
            _ -> return $ IntVal $ evalMulOp op r1 r2
    
    evalExpression (Neg e) = do
        IntVal r <- evalExpression e
        return $ IntVal $ -r

-- -- bool

    evalExpression ELitFalse = return $ BoolVal False
    evalExpression ELitTrue = return $ BoolVal True
    
    evalExpression (Not e) = do
        BoolVal r <- evalExpression e
        return $ BoolVal $ not r

    evalExpression (EAnd e1 e2) = do
        BoolVal r1 <- evalExpression e1
        BoolVal r2 <- evalExpression e2
        return $ BoolVal $ r1 && r2

    evalExpression (EOr e1 e2) = do
        BoolVal r1 <- evalExpression e1
        BoolVal r2 <- evalExpression e2
        return $ BoolVal $ r1 || r2

    evalExpression (ERel e1 op e2) = do
        IntVal r1 <- evalExpression e1
        IntVal r2 <- evalExpression e2
        return $ BoolVal $ evalRelOp op r1 r2

-- -- string

    evalExpression (EString e) = return $ StringVal e

-- -- lambda

    evalExpression (ELambda capture args returnType (Block stmts)) = do
        argsList <- mapM argToFunArg args
        captureGroup <- mapM constructCaptureGroup capture
        return $ FunVal (stmts, Map.empty, argsList, returnType, captureGroup)

-- -- list

    evalExpression (EEmptyList t) = return $ ListVal (t, [])

    evalExpression (ListAt listExpr index) = do
        IntVal i <- evalExpression index
        ListVal (typ, elems) <- evalExpression listExpr
        if i < 0 || i >= fromIntegral (length elems) then
            throwError $ OutOfRangeExeption i
        else
            return $ elems !! fromIntegral i

    evalExpression (ListLength listExpr) = do
        ListVal (typ, elems) <- evalExpression listExpr
        return $ IntVal $ toInteger $ length elems

-- STRUCTS
--    evalExpression EAnonStruct = return $ StructVal Map.empty

    argToFunArg :: Arg -> RR FunArg
    argToFunArg (Arg t (Ident i)) = return (i, t, ByValue)
    argToFunArg (RefArg t (Ident i)) = return (i, t, ByRef)

    constructCaptureGroup :: Ident -> RR CaptureGroupElement
    constructCaptureGroup ident = do
        val <- readFromMemory ident
        return (ident, val)

    -- -- Functions support
    -- arguments list, list of passed expressions or references, current environemnt
    -- returns changed environment
    applyVars :: FunArgList -> [ExprOrRef] -> MyEnv -> RR MyEnv
    applyVars [] [] _ = ask
    applyVars ((ident, typ, ByValue):funArgs) (ERExpr e:vars) curEnv = do
        env <- applyVars funArgs vars curEnv
        val <- local (const curEnv) (evalExpression e)
        local (const env) (declareVar (Ident ident) val)

    applyVars ((ident, typ, ByRef):funArgs) (ERRef ident2:vars) curEnv = do
        env <- applyVars funArgs vars curEnv
        loc <- local (const curEnv) (extractIdentFromMemory ident2)
        let env2 = Map.insert ident loc env
        return env2

    applyCaptureGroup :: CaptureGroup -> RR MyEnv
    applyCaptureGroup [] = ask

    applyCaptureGroup ((ident, val):rest) = do
            env <- applyCaptureGroup rest
            local (const env) (declareVar ident val)

    runFunc :: Expr -> [ExprOrRef] -> RR (MyEnv, ReturnResult)
    runFunc expr vars = do
        curEnv <- ask
        FunVal (stmts, env, funVars, retType, captureGroup) <- evalExpression expr
        env <- local (const env) (applyCaptureGroup captureGroup)
        env <- local (const env) (applyVars funVars vars curEnv)
        (env2, ret) <- local (const env) (interpretMany stmts)
        if retType == Void then
            return (env2, Just VoidVal)
        else
            if isNothing ret then
                throwError NoReturnException
            else
                return (env2, ret)

    loopInList :: Ident -> [MemVal] -> Stmt -> RR (MyEnv, ReturnResult)
    loopInList id [] stmt = returnNothing
    
    loopInList id (val:rest) stmt = do
            putToMemory id val
            execStmt stmt
            loopInList id rest stmt

    defaultValueForType :: Type -> Expr
    defaultValueForType TInt = ELitInt 0
    defaultValueForType TBool = ELitFalse
    defaultValueForType TString = EString ""
    defaultValueForType (TList t) = EEmptyList t
--    defaultValueForType (TStruct (Ident i)) = EAnonStruct

-- Statements

    returnNothing :: RR (MyEnv, ReturnResult)
    returnNothing = do
        env <- ask
        return (env, Nothing)

    memValToString :: MemVal -> String
    memValToString (IntVal i) = show i
    memValToString (BoolVal b) = show b
    memValToString (StringVal s) = s
    memValToString (FunVal (stmt, env, arg, typ, capture)) = "Function (" ++ show arg ++ ") -> " ++ show typ
    memValToString (ListVal (typ, elems)) = "[" ++ (Prelude.foldl (\a b -> a ++ b ++ ", " ) "" (Prelude.map memValToString elems)) ++ "]"
    memValToString e = show e

    formatPrint :: String -> [MemVal] -> RR ()
    formatPrint x [] = liftIO $ putStr x
    formatPrint [] _ = return ()
    formatPrint (chr:chrs) (v:val) =
        if chr == '_' then
            do
                liftIO $ putStr $ memValToString v
                formatPrint chrs val
        else
            do
                liftIO $ putStr [chr]
                formatPrint chrs (v:val)

-- STRUCTS
--    recursiveAssignmentStruct :: StructDef -> Fields -> MemVal -> RR StructDef
--    recursiveAssignmentStruct struct (Field (Ident fieldName)) val = return $ Map.insert fieldName val struct

--    recursiveAssignmentStruct struct (Fields (Ident field) fields) val = do
--            let Just (StructVal subStruct) = Map.lookup field struct
--            changed <- recursiveAssignmentStruct subStruct fields val
--            return $ Map.insert field (StructVal changed) struct

-- -- EXECUTE STATEMENT -- -- 

    execStmt :: Stmt -> RR (MyEnv, ReturnResult)

-- -- Declarations

    execStmt (Decl t (NoInit ident)) = execStmt (Decl t (Init ident (defaultValueForType t)))

    execStmt (Decl (AnonFun retType argsTypes) (Init ident e)) = do
        func <- evalExpression e
        env2 <- declareVar ident func
        return (env2, Nothing)

    execStmt (Decl typ (Init ident e)) = do
        val <- evalExpression e
        env <- declareVar ident val
        return (env, Nothing)

    execStmt (FnDef ident args returnType (Block stmts)) = do
        argsList <- mapM argToFunArg args
        env <- declareVar ident VoidVal
        local (const env) (putToMemory ident (FunVal (stmts, env, argsList, returnType, [])))
        return (env, Nothing)

-- -- assigment

    execStmt (Ass ident e) = do
            newVal <- evalExpression e
            putToMemory ident newVal
            returnNothing

-- STRUCTS
--    execStmt (StructAss ident fields expr) = do
--            val <- evalExpression expr
--            StructVal struct <- readFromMemory ident
--            newStruct <- recursiveAssignmentStruct struct fields val
--            putToMemory ident (StructVal newStruct)
--            returnNothing

-- -- return

    execStmt VRet = do
            env <- ask
            return (env, Just VoidVal)

    execStmt (Ret expr) = do
            env <- ask
            val <- evalExpression expr
            return (env, Just val)
-- -- expr

    execStmt (SExp e) = do
            res <- evalExpression e
            isResVoid <- isVoid res
            isInteractive <- isInterativeMode
            unless (not isInteractive || isResVoid) (do
                            let str = memValToString res
                            liftIO $ putStr str)
            returnNothing

-- -- lists

    execStmt (ListPush ident e) = do
            (typ, elems) <- readListFromMemory ident
            val <- evalExpression e
            putToMemory ident (ListVal (typ, elems ++ [val]))
            returnNothing

    execStmt (ForIn ident list stmt) = do
        ListVal (typ, elems) <- evalExpression list
        loopInList ident elems stmt

-- -- if, while

    execStmt (While expr stmt) = do
            BoolVal res <- evalExpression expr
            if res then 
                do
                    (env, ret) <- execStmt stmt
                    if isNothing ret then
                        execStmt (While expr stmt)
                    else
                        return (env, ret)
            else 
                returnNothing

    execStmt (For ident start end body) = do
            IntVal s <- evalExpression start
            IntVal e <- evalExpression end
            execStmt $ BStmt $ Block [Ass ident (ELitInt s), While (ERel (EVar ident) LE (ELitInt e)) (BStmt $ Block [body, Ass ident (EAdd (EVar ident) Plus (ELitInt 1))])]

-- STRUCTS
--    execStmt (ForField ident fields start end body) = do
--            IntVal s <- evalExpression start
--            IntVal e <- evalExpression end
--            execStmt $ BStmt $ Block [StructAss ident fields (ELitInt s), While (ERel (EStructField2 (EVar ident) fields) LE (ELitInt e)) (BStmt $ Block [body, StructAss ident fields (EAdd (EStructField2 (EVar ident) fields) Plus (ELitInt 1))])]

    execStmt (Cond expr (Block stmts)) = do
            BoolVal res <- evalExpression expr
            if res then 
                interpretMany stmts
            else 
                returnNothing

    execStmt (CondElse expr (Block stmts) (Block stmtsElse)) = do
            BoolVal res <- evalExpression expr
            if res then interpretMany stmts else interpretMany stmtsElse

-- -- structs

    -- execStmt (StructDef (Ident i) fields) = do


-- -- print

    execStmt (Print e) = do
            res <- evalExpression e
            let str = memValToString res
            liftIO $ putStr str
            returnNothing

    execStmt (SFormat format exprs) = do
        values <- mapM evalExpression exprs
        formatPrint format values
        returnNothing

-- -- block

    execStmt (BStmt (Block stmts)) = do
            env <- ask
            (env2, ret) <- interpretMany stmts
            return (env, ret)

    interpretMany :: [Stmt] -> RR (MyEnv, ReturnResult)
    interpretMany (s:xs) = do
                (env, ret) <- execStmt s
                if isNothing ret then
                    local (const env) (interpretMany xs)
                else
                    return (env, ret)
    interpretMany [] = returnNothing

    runMyMonad prog mode = runExceptT $ runStateT (runReaderT (interpretMany prog) Map.empty) (Map.empty, 0, mode)
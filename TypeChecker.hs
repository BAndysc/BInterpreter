module TypeChecker where
    import AbsGrammar

    import Data.Map as Map
    import Data.Maybe

    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except

    import Types
    import Memory

    -- Type checker env. Declared here, as it is only used in this file
    type TTEnv = Map.Map VariableName Type
    type TT = ReaderT TTEnv (ExceptT TypeCheckExceptions IO)

    type TTReturnResult = Maybe Type

    ensureType :: Type -> Type -> TT ()
    ensureType givenType expectedType = unless (givenType == expectedType) $ throwError $ TypeCheckException givenType expectedType

    ensureTypeExpr :: Expr -> Type -> TT ()
    ensureTypeExpr e t = do
        typ <- evalExprType e
        ensureType typ t

-- helpers

    getTypeFromArg :: Arg -> TT TypeOrRef
    getTypeFromArg (Arg typ ident) = return $ TypeOrRefType typ
    getTypeFromArg (RefArg typ ident) = return $ TypeOrRefRef typ


    addArgsTypesToEnv :: [Arg] -> TT TTEnv
    addArgsTypesToEnv [] = ask
    addArgsTypesToEnv (Arg typ (Ident ident):args) = do
        env <- addArgsTypesToEnv args
        return $ Map.insert ident typ env

    addArgsTypesToEnv (RefArg typ (Ident ident):args) = do
        env <- addArgsTypesToEnv args
        return $ Map.insert ident typ env

    addCaptureGroupTypesToEnv :: [Ident] -> TT TTEnv
    addCaptureGroupTypesToEnv [] = ask
    addCaptureGroupTypesToEnv (Ident ident: idents) = do
        env <- addCaptureGroupTypesToEnv idents
        typ <- getTypeFromEnv (Ident ident)
        return $ Map.insert ident typ env

    ensureArgsTypes :: [TypeOrRef] -> [ExprOrRef] -> TT ()
    ensureArgsTypes [] [] = return ()
    ensureArgsTypes (TypeOrRefType typ:restTypes) (ERExpr expr:vars) = do
        ensureTypeExpr expr typ
        ensureArgsTypes restTypes vars

    ensureArgsTypes (TypeOrRefRef typ:restTypes) (ERRef ident:vars) = do
        identType <- getTypeFromEnv ident
        ensureType identType typ
        ensureArgsTypes restTypes vars
    
    ensureArgsTypes a b = throwError FuncApplicationException 

    evalExprType :: Expr -> TT Type

    evalExprType (EVar ident) = getTypeFromEnv ident

    evalExprType (EApp expr vars) = do
        (AnonFun retType argTypes) <- evalExprType expr
        ensureArgsTypes argTypes vars
        return retType

    --evalExprType (EStructField2 expr field) = do
    --    StructVal val <- evalExpression expr
    --    extractFieldFromStructHelper val field

    evalExprType ELitInt {} = return TInt
    evalExprType (EAdd e1 op e2) = do
        ensureTypeExpr e1 TInt
        ensureTypeExpr e2 TInt
        return TInt

    evalExprType (EMul e1 op e2) = do
        ensureTypeExpr e1 TInt
        ensureTypeExpr e2 TInt
        return TInt

    evalExprType (EAnd e1 e2) = do
        ensureTypeExpr e1 TBool
        ensureTypeExpr e2 TBool
        return TBool

    evalExprType (EOr e1 e2) = do
        ensureTypeExpr e1 TBool
        ensureTypeExpr e2 TBool
        return TBool
    
    evalExprType (Neg e) = do
        ensureTypeExpr e TInt
        return TInt

    evalExprType ELitFalse = return TBool
    evalExprType ELitTrue = return TBool

    evalExprType (ERel e1 op e2) = do
        ensureTypeExpr e1 TInt
        ensureTypeExpr e2 TInt
        return TBool
    
    evalExprType (Not e) = do
        ensureTypeExpr e TBool
        return TBool

    evalExprType (EString e) = return TString

-- -- lambda
    evalExprType (ELambda capture args returnType (Block stmts)) = do
        newEnv <- addCaptureGroupTypesToEnv capture
        newEnv <- local (const newEnv) (addArgsTypesToEnv args)
        (env, typ) <- local (const newEnv) (typeCheckMany stmts)

        if isNothing typ then
            if returnType == Void then
                typeCheckOk
            else
                throwError $ TypeCheckException Void returnType
        else
            do
                let Just typUnpacked = typ
                ensureType typUnpacked returnType
                typeCheckOk

        types <- mapM getTypeFromArg args
        return $ AnonFun returnType types

-- -- list

    evalExprType (EEmptyList t) = return $ TList t

    evalExprType (ListAt list expr) = do
        typ <- evalExprType expr
        TList typ <- evalExprType list
        return typ
    
    evalExprType (ListLength expr) = do
        typ <- evalExprType expr
        isList <- isTypeList typ
        if isList then
            return TInt
        else
            throwError $ NotAListException typ

    isTypeList :: Type -> TT Bool
    isTypeList TList {} = return True
    isTypeList _ = return False

-- -- struct
-- TODO ident
--    evalExprType EAnonStruct = return $ TStruct (Ident "")

    getTypeFromEnv :: Ident -> TT Type
    getTypeFromEnv (Ident i) = do
        env <- ask
        case Map.lookup i env of
            Nothing -> throwError $ NonexistingIdentifierException i
            Just typ -> return typ

    typeCheckOk :: TT (TTEnv, TTReturnResult)
    typeCheckOk = do
        env <- ask
        return (env, Nothing)

    -- Type -> Is Initialized -> Is Valid
    isValidVarType :: Type -> Bool -> TT Bool
    isValidVarType TInt _ = return True
    isValidVarType TBool _ = return True
    isValidVarType TString _ = return True
    isValidVarType TList {} _ = return True
    isValidVarType AnonFun {} isInitialized = return isInitialized
    isValidVarType _ _ = return False

    typeCheck :: Stmt -> TT (TTEnv, TTReturnResult)
    typeCheck (Decl typ (NoInit (Ident i))) = do
        isValid <- isValidVarType typ False
        unless isValid $ throwError $ InvalidTypeInDeclarationException typ
        env <- ask
        return (Map.insert i typ env, Nothing)

    typeCheck (Decl typ (Init (Ident i) e)) = do
        isValid <- isValidVarType typ True
        unless isValid $ throwError $ InvalidTypeInDeclarationException typ
        ensureTypeExpr e typ
        env <- ask
        return (Map.insert i typ env, Nothing)
        
    typeCheck (Ass id e) = do
        typ <- getTypeFromEnv id
        ensureTypeExpr e typ
        typeCheckOk

    typeCheck (FnDef (Ident ident) args retType (Block stmts)) = do
        types <- mapM getTypeFromArg args
        let funDef = AnonFun retType types
        newEnv <- addArgsTypesToEnv args
        let newEnv2 = Map.insert ident funDef newEnv
        (env, typ) <- local (const newEnv2) (typeCheckMany stmts)
        if isNothing typ then
            if retType == Void then
                do
                    env <- ask
                    return (Map.insert ident funDef env, Nothing)
            else
                throwError $ TypeCheckException Void retType
        else
            do
                let Just typUnpacked = typ
                ensureType typUnpacked retType
                env <- ask
                return (Map.insert ident funDef env, Nothing)

-- conds, while, block

    typeCheck (BStmt (Block stmts)) = typeCheckMany stmts

    typeCheck (Cond expr (Block stmts)) = do
        ensureTypeExpr expr TBool
        typeCheckMany stmts

    typeCheck (CondElse expr (Block stmts) (Block stmtsElse)) = do
        ensureTypeExpr expr TBool
        (env1, mRet1) <- typeCheckMany stmts
        (env2, mRet2) <- typeCheckMany stmtsElse
        case (mRet1, mRet2) of
            (Nothing, Nothing) -> typeCheckOk
            (Just a, Just b) -> do
                        ensureType a b
                        env <- ask
                        return (env, mRet1) -- ret1 === ret2
            (_, _) -> do
                throwError $ ReturnTypeMismatchException mRet1 mRet2
                typeCheckOk

    typeCheck (While expr stmt) = do
        ensureTypeExpr expr TBool
        typeCheck stmt

    typeCheck (ForIn ident expr stmt) = do
        TList listType <- evalExprType expr
        acumulator <- getTypeFromEnv ident
        ensureType acumulator listType
        typeCheck stmt

    typeCheck (For ident start end stmt) = do
        ensureTypeExpr start TInt
        ensureTypeExpr end TInt
        identType <- getTypeFromEnv ident
        ensureType identType TInt
        typeCheck stmt

-- return

    typeCheck VRet = do
        env <- ask
        return (env, Just Void)

    typeCheck (Ret expr) = do
        env <- ask
        val <- evalExprType expr
        return (env, Just val)

-- -- lists

    typeCheck (ListPush ident expr) = do
        TList typ <- getTypeFromEnv ident
        ensureTypeExpr expr typ
        typeCheckOk

-- -- print, sexpr

    typeCheck (SExp e) = do
        evalExprType e
        typeCheckOk

    typeCheck (Print expr) = do
        evalExprType expr
        typeCheckOk

    typeCheck (SFormat format exprs) = do
        evaled <- mapM evalExprType exprs
        typeCheckOk

    -- typeCheck 
    typeCheckMany :: [Stmt] -> TT (TTEnv, TTReturnResult)
    typeCheckMany (s:xs) = do
        (env, ret) <- typeCheck s
        if isNothing ret then
            local (const env) (typeCheckMany xs)
        else
            return (env, ret)
    typeCheckMany [] = do
        env <- ask
        return (env, Nothing)


    runTC prog = runExceptT $ runReaderT (typeCheckMany prog) Map.empty
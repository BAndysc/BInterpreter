module Memory where
    import AbsGrammar

    import Data.Map as Map
    import Data.Maybe

    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except

    import Types
    
    -- type check should detect Nothing

    declareVar :: Ident -> MemVal -> RR MyEnv
    declareVar (Ident i) memval = do
        env <- ask
        (store, loc, mode) <- get 
        let env2 = Map.insert i loc env
        put (Map.insert loc memval store, loc+1, mode)
        return env2

    putToMemory :: Ident -> MemVal -> RR ()
    putToMemory (Ident i) memval = do      
        env <- ask
        let Just location = Map.lookup i env
        (store, loc, mode) <- get
        let Just curVal = Map.lookup location store
        put (Map.insert location memval store, loc, mode)
        return ()

    extractIdentFromMemory :: Ident -> RR Loc
    extractIdentFromMemory (Ident i) = do
        env <- ask
        let Just location = Map.lookup i env
        return location

    readFromMemory :: Ident -> RR MemVal
    readFromMemory ident = do
        loc <- extractIdentFromMemory ident
        (store, curLoc, _) <- get
        let Just val = Map.lookup loc store
        return val

    readFuncFromMemory :: Ident -> RR FuncDef
    readFuncFromMemory ident = do
        FunVal fun <- readFromMemory ident
        return fun

    readListFromMemory :: Ident -> RR ListDef
    readListFromMemory ident = do
        ListVal list <- readFromMemory ident
        return list

    isVoid :: MemVal -> RR Bool
    isVoid VoidVal = return True
    isVoid _ = return False

    isInterativeMode :: RR Bool
    isInterativeMode = do
        (_, _, mode) <- get
        return $ mode == Interactive

-- STRUCTS
--     extractFieldFromStructHelper :: StructDef -> Fields -> RR MemVal
--     extractFieldFromStructHelper struct (Field (Ident fieldName)) =
--         case Map.lookup fieldName struct of
--             Just val -> return val
--             Nothing -> throwError $ NoStructFieldException fieldName
-- 
--     extractFieldFromStructHelper struct (Fields (Ident fieldName) fields) =
--         case Map.lookup fieldName struct of
--             Just (StructVal val) -> extractFieldFromStructHelper val fields
--             Nothing -> throwError $ NoStructFieldException fieldName
-- 
--     extractFieldFromStruct :: Ident -> Fields -> RR MemVal
--     extractFieldFromStruct ident fields = do
--         StructVal struct <- readFromMemory ident
--         extractFieldFromStructHelper struct fields
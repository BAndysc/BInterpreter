module Main where
    import System.IO ( stdin, hGetContents, hPutStrLn, stderr, getContents, hPutStr )
    import System.Environment ( getArgs, getProgName )
    import System.Exit ( exitFailure, exitSuccess )
    import Control.Exception (catch, IOException)

    import Interpreter
    import TypeChecker
    import Types

    import ParGrammar
    import AbsGrammar
    import ErrM

    exitWithError :: String -> IO ()
    exitWithError msg = do
        hPutStrLn stderr msg
        exitFailure

    parse :: InterpreterMode -> String -> IO ()
    parse mode input =
        case pProgram (myLexer input) of
            (Ok s) -> do
                let Program program = s
                typeCheckResult <- runTC program
                case typeCheckResult of
                    Left err -> do
                        hPutStr stderr "Typecheck error. "        
                        case err of
                            TypeCheckException given expected -> exitWithError $ "Expected " ++ show expected ++ " given: "++ show given
                            FuncApplicationException -> exitWithError "Invalid function argument application"
                            NonexistingIdentifierException i -> exitWithError $ "Identifier " ++ i ++ " doesn't exist"
                            ReturnTypeMismatchException r1 r2 -> exitWithError $ "Return type mismatch: " ++ show r1 ++ " and " ++ show r2
                            NotAListException typ -> exitWithError $ show typ ++ " should be a List, but is not"
                    Right _ -> do
                        runTimeResult <- runMyMonad program mode
                        case runTimeResult of
                            Left err -> do
                                hPutStr stderr "Runtime exception. "
                                case err of
                                    DivisionByZeroException -> exitWithError "Division by 0"
                                    ModulusByZeroException -> exitWithError "Modulo 0"
                                    NoReturnException ->  exitWithError "Function didn't return any value"
                                    NoStructFieldException fieldName -> exitWithError ("Field " ++ fieldName ++ " doesn't exist in struct")
                                    OutOfRangeExeption i -> exitWithError ("Index " ++ show i ++ " out of range!")
                            Right _ -> return()
                return ()
            (Bad s) -> exitWithError "Parse error"

    parseFile :: String -> IO ()
    parseFile fileName = readFile fileName >>= parse NonInteractive

    main :: IO ()
    main = do
        args <- getArgs
        case args of
            [] -> getContents >>= parse Interactive
            files -> mapM_ parseFile files

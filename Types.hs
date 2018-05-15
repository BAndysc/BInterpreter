module Types where
    import AbsGrammar

    import Data.Map as Map
    import Data.Maybe

    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except

    type VariableName = String

    type StructFieldName = String

    type Loc = Integer

    -- interpreter mode, in interactive, SExpr is printed to stdout
    data InterpreterMode = Interactive | NonInteractive deriving Eq


    -- Environment, maps variable/function name to location
    type MyEnv = Map.Map VariableName Loc

    -- return value of block of statements. Nothing when no return, Just MemVal if return occured 
    type ReturnResult = Maybe MemVal

    -- way of passing argument to function
    data PassArgType = ByValue | ByRef deriving Show 

    -- argument passed to function (name of variable, type of variable, way of passing argument)
    type FunArg = (VariableName, Type, PassArgType)

    -- list of arguments passed to function
    type FunArgList = [FunArg]

    -- type extended with information if it is reference or not
    type TypeRef = (Type, PassArgType)

    -- list of "capture group" - name of variable and value when created lambda
    type CaptureGroupElement = (Ident, MemVal) 
    type CaptureGroup = [CaptureGroupElement]

    -- function/lambda - fun body, environment when declared function, return type, capture group when declared function/lambda (function always empty)
    type FuncDef = ([Stmt], MyEnv, FunArgList, Type, CaptureGroup)

    -- list definition - hold type and list of values
    type ListDef = (Type, [MemVal])

    -- struct definition - field name and its value
    type StructDef = Map.Map StructFieldName MemVal

    -- struct description - field name and its type
    type StructDesc = Map.Map StructFieldName Type

    -- general type for value in memory (in store)
    data MemVal = BoolVal Bool | IntVal Integer | StringVal String | VoidVal | FunVal FuncDef | ListVal ListDef | StructVal StructDef | StructDescVal StructDesc deriving Show

    -- "memory"
    type MyState = Map.Map Loc MemVal

    -- tuple, State ("memory"), next free location and interpreter mode
    type MyStore = (MyState, Loc, InterpreterMode)

    data TypeCheckExceptions = NotAListException Type | TypeCheckException Type Type | FuncApplicationException | NonexistingIdentifierException String | ReturnTypeMismatchException (Maybe Type) (Maybe Type)  deriving Show

    data RuntimeExceptions = DivisionByZeroException | ModulusByZeroException | NoReturnException | NoStructFieldException String | OutOfRangeExeption Integer deriving Show

    -- Monad type
    type RR = ReaderT MyEnv (StateT MyStore (ExceptT RuntimeExceptions IO))
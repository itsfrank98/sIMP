module Interpreter where
import Grammar(AExpr(..), BExpr(..), Com(..), Values(..))
import Utils(readArray, power, ratio, createEmptyArray, writeArray, removeElem)

data Variable = Variable {
    var_name:: String,
    var_type:: String,
    var_value:: Maybe(Values)}

instance Show Variable where
    show (Variable name t val) = "\t" ++ show name ++ "   \t" ++ show t ++ "   \t" ++ show val ++ "\n"

type Env = [Variable]

--This is the data returned by evalAexpr, evalArrayOperation and evalBExpr. If the evaluation was successful, then the result will be put into the
-- 'result' field. If some errors occured, then the error message will be returned 
data Output a =
    Result a
    | Error String

--------- IMPLEMENTATION OF FUNCTOR, APPLICATIVE AND EQ FOR THE OUTPUT DATATYPE ---------
instance Functor Output where
    fmap _ (Error a) = Error a
    fmap f (Result a) = Result (f a)

instance Applicative Output where
    pure = Result
    (Result f) <*> (Result j) = Result (f j)

instance (Eq a) => Eq (Output a) where
    Result r1 == Result r2 = r1 == r2
    Error e == Result r = False
    Result r == Error e = False
    Error e1 == Error e2 = e1 == e2
-----------------------------------------------------------------------------------------

{-- OutputEnv is the data returned by commandExec and programExec. It is basically the same as the output datatype, but in this case the ErrorEnv field contains
 both the error message and the command which caused the error to happen. -}
data OutputEnv =
    ResultEnv Env
    | ErrorEnv (String, Com)

instance Show OutputEnv where
    show (ResultEnv e) = show e
    show (ErrorEnv (msg, com)) = "*** ERROR !!! " ++ show msg ++ " !!!***\n ON COMMAND: " ++ show com

-- Given an env and a variable, if the variable already exists in the env, its value is overwritten. Otherwise, the variable is added. The updated env is returned
modifyEnv :: Env -> Variable -> Env
modifyEnv [] newVar = [newVar]
modifyEnv (x:xs) newVar =  if (var_name x) == (var_name newVar) && (var_type x) == (var_type newVar)
    then [newVar] ++ xs
    else [x] ++ modifyEnv xs newVar

-- Given a variable name and its type, returns the corresponding variable.
readEnv :: Env -> String -> String -> Maybe(Variable)
readEnv [] n t = Nothing
readEnv (x:xs) n t = if (var_name x) == n && (var_type x) == t
    then Just x
    else readEnv xs n t

-- Given a variable name and its type, returns (if exists) the position that the variable has inside the env list
-- findVariable [Variable {var_name = "a", var_type = "int", var_value =  (Just(Integer 6))}, Variable {var_name = "a", var_type = "array", var_value =  (Just (Array [(Const 1)]))}] "v" "b"
findVariable :: Env -> String -> String -> Int
findVariable [] _ _ = error "Variable not found"
findVariable (x:xs) n t = if (var_name x) == n && (var_type x) == t
    then 0
    else (+1)(findVariable xs n t)

-- evalAexpr (Ar "b" (Const 4)) [Variable{var_name="v",var_type= "b",var_value=(Just(Integer 3))}] FAIL
-- evalAexpr (Ar "b" (Const 4)) [Variable{var_name="b",var_type= "array",var_value=(Just (Array [3,4,5,4,5]))}] SUCCESS
-- evalAexpr (ArithmeticIdentifier "b") [Variable{var_name="b",var_type= "int",var_value= Just(Integer 4)}]
evalAexpr :: AExpr -> Env -> Output Int
evalAexpr (Const k) _ = Result k
evalAexpr (Ar name pos) env = 
    case (readEnv env name "array") of
        Nothing -> Error "The array does not exist"
        Just (v) -> case (var_value v) of
            Nothing -> Error "You are trying to read from an empty array"
            Just(Array v) -> Result (readArray v p)
                        where Result p = (evalAexpr pos env)
evalAexpr (ArithmeticIdentifier name) env =
    case (readEnv env name "int") of
        Nothing -> Error "The integer variable does not exist"
        Just (v) -> case (var_value v) of
            Nothing -> Error "Empty variable"
            Just(Integer i) -> Result i
evalAexpr (Top name) env =
    case (readEnv env name "stack") of
        Nothing -> Error "The stack does not exist"
        Just (v) -> case (length s == 0) of
            True -> Error "Empty stack!"
            False -> Result (readArray s ((length s) - 1))
            where Just (Stack s) = (var_value v)
evalAexpr (Add a b) env = (+) <$> (evalAexpr a env) <*> (evalAexpr b env)
evalAexpr (Diff a b) env = (-) <$> (evalAexpr a env) <*> (evalAexpr b env)
evalAexpr (Div a b) env = if (evalAexpr b env) == Result 0
    then error "Division by zero"
    else (ratio) <$> (evalAexpr a env) <*> (evalAexpr b env)
evalAexpr (Prod a b) env = (*) <$> (evalAexpr a env) <*> (evalAexpr b env)
evalAexpr (Power a b) env = (power) <$> (evalAexpr a env) <*> (evalAexpr b env)

-- Evaluation of the operations that return an entire array
evalArrayOperation :: AExpr -> Env -> Output [Int]
evalArrayOperation (ConstArr ar) _ = Result ar
evalArrayOperation (ArithmeticIdentifier name) env =
    case (readEnv env name "array") of
        Nothing -> Error "The array does not exist"
        Just (v) -> case (var_value v) of
            Nothing -> Error "Empty array"
            Just(Array v) -> Result v

evalBExpr :: BExpr -> Env -> Output Bool
evalBExpr (BVal b) _ = Result b
evalBExpr (BooleanIdentifier name) env =
    case (readEnv env name "bool") of
        Nothing -> Error "The boolean variable does not exist"
        Just(v) -> case (var_value v) of
            Nothing -> Error "Empty variable"
            Just(Boolean b) ->Result b
evalBExpr (Empty name) env = case (readEnv env name "stack") of
        Nothing -> Error "The stack does not exist"
        Just (v) -> Result (length s == 0) 
            where Just (Stack s) = (var_value v)
evalBExpr (And a b) env = (&&) <$> (evalBExpr a env) <*> (evalBExpr b env)
evalBExpr (Or a b) env = (||) <$> (evalBExpr a env) <*> (evalBExpr b env)
evalBExpr (Not a) env = not <$> (evalBExpr a env)
evalBExpr (Lt a b) env = (<) <$> (evalAexpr a env) <*> (evalAexpr b env)
evalBExpr (Gt a b) env = (>) <$> (evalAexpr a env) <*> (evalAexpr b env)
evalBExpr (Eq a b) env = (==) <$> (evalAexpr a env) <*> (evalAexpr b env)
evalBExpr (Different a b) env = not <$> (evalBExpr (Eq a b) env)
evalBExpr (Lte a b) env = (<=) <$> (evalAexpr a env) <*> (evalAexpr b env)
evalBExpr (Gte a b) env = (>=) <$> (evalAexpr a env) <*> (evalAexpr b env)

commandExec :: Com -> Env -> OutputEnv
commandExec (DeclareBoolean name expr) env =
    case (readEnv env name "bool") of
        Just(v) -> ErrorEnv("The variable has already been declared", (DeclareBoolean name expr))
        Nothing -> case expr of
            Nothing -> ResultEnv (modifyEnv env Variable{var_name = name, var_type = "bool", var_value = Nothing})
            Just(v) -> case (evalBExpr v env) of
                Result b -> ResultEnv (modifyEnv env Variable{var_name = name, var_type = "bool", var_value = (Just(Boolean b))})
                Error a -> ErrorEnv (a, (DeclareBoolean name expr))
commandExec (DeclareInteger name expr) env =
    case (readEnv env name "int") of
        Just(v) -> ErrorEnv ("The variable has already been declared", (DeclareInteger name expr))
        Nothing -> case expr of
            Nothing -> ResultEnv (modifyEnv env Variable{var_name = name, var_type = "int", var_value = Nothing})
            Just(v) -> case (evalAexpr v env) of
                Result i -> ResultEnv (modifyEnv env Variable{var_name = name, var_type = "int", var_value = (Just(Integer i))})
                Error a -> ErrorEnv (a, (DeclareInteger name expr))
-- commandExec(DeclareArray "b" (Const 2) (Just( ConstArr [1, 2])))[Variable {var_name = "a", var_type = "array", var_value =  (Just (Array [1]))}] SUCCESS
commandExec (DeclareArray name dim val) env =
    case (readEnv env name "array") of
        Just(v) -> ErrorEnv ("The array has already been declared", (DeclareArray name dim val))
        Nothing -> case val of
            Nothing -> ResultEnv (modifyEnv env Variable{var_name = name, var_type = "array", var_value = (Just(Array(createEmptyArray s)))})
            Just(ConstArr arr) -> case (length arr == s) of
                True -> ResultEnv (modifyEnv env Variable {var_name = name, var_type = "array", var_value = (Just(Array ar))}) 
                                                    where Result ar = (evalArrayOperation (ConstArr arr) env)
                False -> ErrorEnv ("The given dimension and the actual length are not equal", (DeclareArray name dim val))
            where Result s = (evalAexpr dim env)
commandExec (DeclareStack name) env =
    case (readEnv env name "stack") of
        Just(v) -> ErrorEnv ("The array has already been declared", (DeclareStack name))
        Nothing -> ResultEnv (modifyEnv env Variable {var_name = name, var_type = "stack", var_value = (Just(Stack []))})
-- commandExec (AssignBoolean "a" (BVal True)) [Variable {var_name = "a", var_type = "array", var_value =  (Just (Array [(Const 1)]))}]
commandExec (AssignBoolean name value) env =
    case (readEnv env name "bool") of
        Nothing -> ErrorEnv ("The boolean does not exist", (AssignBoolean name value))
        Just(v) -> case (evalBExpr value env) of
            Result b -> ResultEnv (modifyEnv env Variable{var_name = name, var_type = "bool", var_value = (Just(Boolean b))})
            Error a -> ErrorEnv (a, (AssignBoolean name value))
-- commandExec (AssignInteger "a" (Const 2)) [Variable {var_name = "a", var_type = "int", var_value = Just(Integer 5)}]
commandExec (AssignInteger name value) env =
    case (readEnv env name "int") of
        Nothing -> ErrorEnv ("The integer does not exist", (AssignInteger name value))
        Just(v) -> case (evalAexpr value env) of
            Error a -> ErrorEnv (a, (AssignInteger name value))
            Result i -> ResultEnv (modifyEnv env Variable{var_name = name, var_type = "int", var_value = (Just(Integer i))})
-- commandExec(AssignArrayPosition "a" (Const 0) (Const 1))[Variable {var_name = "a", var_type = "array", var_value = (Just (Array [14,15]))}]
commandExec (AssignArrayPosition name pos val) env =    --I don't need to check if the array exists because evalArrayOperation already takes care of that
    case (evalAexpr val env) of                  -- Evaluation the value to be put into the array
        Result r -> case (evalAexpr pos env) of         -- Evaluation of the index
            Result p -> case (evalArrayOperation (ArithmeticIdentifier name) env) of        -- Then I evaluate the identifier in order to retrieve the array
                Result ar -> case (writeArray ar p r) of        -- Here I try to write in the array
                    Just (v) -> ResultEnv (modifyEnv env Variable{var_name = name, var_type = "array", var_value = (Just(Array v))})
                    Nothing -> ErrorEnv ("Index out of range", (AssignArrayPosition name pos val))
            Error a -> ErrorEnv (a, (AssignArrayPosition name pos val))
        Error b -> ErrorEnv (b, (AssignArrayPosition name pos val))
--commandExec (AssignWholeArray "a" (ConstArr [3, 4]))[Variable {var_name = "a", var_type = "array", var_value = (Just (Array [14, 21]))}]
commandExec (AssignWholeArray name ar) env = 
    case (evalArrayOperation (ArithmeticIdentifier name) env) of        -- Returns the array which we are trying to substitute in the env
        Result v -> case (evalArrayOperation ar env) of                 -- Evaluates the array which we are trying to insert in the env
            Result arr -> case (length v == length arr) of
                True -> ResultEnv (modifyEnv env Variable{var_name = name, var_type = "array", var_value = (Just(Array arr))})
                False -> ErrorEnv ("Mismatching length", (AssignWholeArray name ar))
            Error e1 -> ErrorEnv (e1, (AssignWholeArray name ar))
        Error e2 -> ErrorEnv (e2, (AssignWholeArray name ar))
--commandExec (Push "a" (Const 0))[Variable {var_name = "a", var_type = "stack", var_value = (Just (Stack [14, 21]))}]
commandExec (Push name val) env =
    case (readEnv env name "stack") of
        Nothing -> ErrorEnv ("The stack does not exist", (Push name val))
        Just(v) -> case (evalAexpr val env) of
            Error a -> ErrorEnv (a, (Push name val))
            Result i -> ResultEnv (modifyEnv env Variable {var_name = name, var_type = "stack", var_value = (Just(Stack (s ++ [i])))})
                        where Just (Stack s) = (var_value v)
--commandExec (Pop "a")[Variable {var_name = "a", var_type = "stack", var_value = (Just (Stack [14, 21]))}]
commandExec (Pop name) env =
    case (readEnv env name "stack") of
        Nothing -> ErrorEnv ("The stack does not exist", (Pop name))
        Just(v) -> case (length s == 0) of
            True -> ErrorEnv ("Can't pop from an empty stack!", (Pop name))
            False -> ResultEnv (modifyEnv env Variable {var_name = name, var_type = "stack", var_value = (Just(Stack (removeElem s ((length s)-1))))})
            where Just (Stack s) = (var_value v)

programExec :: [Com] -> Env -> OutputEnv
programExec [] env = ResultEnv env
programExec ((DeclareBoolean name expr) : cs) env = case (commandExec (DeclareBoolean name expr) env) of
    ErrorEnv (e, c) -> ErrorEnv (e, c)
    ResultEnv new_env -> programExec cs new_env
programExec ((DeclareInteger name expr) : cs) env = case (commandExec (DeclareInteger name expr) env) of
    ErrorEnv (e, c) -> ErrorEnv (e, c)
    ResultEnv new_env -> programExec cs new_env
programExec ((DeclareArray name dim val) : cs) env = case (commandExec (DeclareArray name dim val) env) of
    ErrorEnv (e, c) -> ErrorEnv (e, c)
    ResultEnv new_env -> programExec cs new_env
programExec ((DeclareStack name) : cs) env = case (commandExec (DeclareStack name) env) of
    ErrorEnv (e, c) -> ErrorEnv (e, c)
    ResultEnv new_env -> programExec cs new_env
programExec ((AssignBoolean name value) : cs) env = case (commandExec (AssignBoolean name value) env) of
    ErrorEnv (e, c) -> ErrorEnv (e, c)
    ResultEnv new_env -> programExec cs new_env
programExec ((AssignInteger name value) : cs) env = case (commandExec (AssignInteger name value) env) of
    ErrorEnv (e, c) -> ErrorEnv (e, c)
    ResultEnv new_env -> programExec cs new_env
programExec ((AssignArrayPosition name pos val) : cs) env = case (commandExec (AssignArrayPosition name pos val) env) of
    ErrorEnv (e, c) -> ErrorEnv (e, c)
    ResultEnv new_env -> programExec cs new_env
programExec ((AssignWholeArray name ar) : cs) env = case (commandExec (AssignWholeArray name ar) env) of
    ErrorEnv (e, c) -> ErrorEnv (e, c)
    ResultEnv new_env -> programExec cs new_env
programExec ((Push name value) : cs) env = case (commandExec (Push name value) env) of
    ErrorEnv (e, c) -> ErrorEnv (e, c)
    ResultEnv new_env -> programExec cs new_env
programExec ((Pop name) : cs) env = case (commandExec (Pop name) env) of
    ErrorEnv (e, c) -> ErrorEnv (e, c)
    ResultEnv new_env -> programExec cs new_env    
programExec ((Ifelse cond progA progB) : cs) env = 
    case (evalBExpr cond env) of
        Error a -> ErrorEnv (a, (Ifelse cond progA progB))
        Result True -> programExec (progA ++ cs) env
        Result False ->
            case progB of
                Nothing -> programExec cs env
                Just(com) -> programExec (com ++ cs) env
programExec ((Whiledo cond prog) : cs) env =
    case (evalBExpr cond env) of
        Error a -> ErrorEnv (a, (Whiledo cond prog))
        Result True -> programExec (prog ++ [Whiledo cond prog] ++ cs) env
        Result False -> programExec cs env
programExec ((Dowhile prog cond) : cs) env =
    case (programExec prog env) of
        ErrorEnv (msg, com) -> ErrorEnv (msg, (Dowhile prog cond))
        ResultEnv env -> case (evalBExpr cond env) of
            Error a -> ErrorEnv (a, (Dowhile prog cond))
            Result True -> programExec ([Dowhile prog cond] ++ cs) env
            Result False -> programExec cs env
programExec (Skip : cs) env = programExec cs env

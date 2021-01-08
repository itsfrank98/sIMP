module Grammar where

data Values =
    Integer Int
    | Boolean Bool
    | Array [Int]
    | Stack [Int]

instance Show Values where
    show (Integer a) = "int " ++ show a
    show (Boolean b) = "bool " ++ show b
    show (Array ar) = "array " ++ show ar
    show (Stack st) = "stack " ++ show st

--Arithmetic expressions
data AExpr =
    Const Int
    | ConstArr [Int]
    | Ar String AExpr
    | ArithmeticIdentifier String
    | Top String
    | Add AExpr AExpr
    | Diff AExpr AExpr
    | Div AExpr AExpr
    | Prod AExpr AExpr
    | Power AExpr AExpr
    deriving Show

-- Boolean expressions
data BExpr =
    BVal Bool
    | BooleanIdentifier String
    | Empty String
    | And BExpr BExpr
    | Or BExpr BExpr
    | Not BExpr
    | Lt AExpr AExpr
    | Gt AExpr AExpr
    | Eq AExpr AExpr
    | Different AExpr AExpr
    | Lte AExpr AExpr
    | Gte AExpr AExpr
    deriving Show

-- Commands
data Com = DeclareBoolean String (Maybe BExpr)
    | DeclareInteger String (Maybe AExpr)
    | DeclareArray String AExpr (Maybe AExpr)
    | DeclareStack String
    | AssignBoolean String BExpr
    | AssignInteger String AExpr
    | AssignArrayPosition String AExpr AExpr --Assign a value to a specific array cell. Example: array[4] = 5
    | AssignWholeArray String AExpr --Assign whole array to a pre-declared variable. Example: a = [1,2,3,4]
    | Push String AExpr
    | Pop String 
    | Ifelse BExpr Program (Maybe Program)
    | Whiledo BExpr Program
    | Dowhile Program BExpr
    | Skip
    deriving Show

type Program = [Com]
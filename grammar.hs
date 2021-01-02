module Grammar where

data Values =
    Integer Int
    | Boolean Bool
    | Array [Int]

instance Show Values where
    show (Integer a) = "Integer " ++ show a
    show (Boolean b) = "Boolean " ++ show b
    show (Array ar) = "Array " ++ show ar

--Arithmetic expressions
data AExpr =
    Const Int
    | ConstArr [Int]
    | Ar String AExpr
    | ArithmeticIdentifier String
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
    | AssignBoolean String BExpr
    | AssignArrayPosition String AExpr AExpr --Assign a value to a specific array cell. Example: array[4] = 5
    | AssignWholeArray String AExpr --Assign whole array to a pre-declared variable. Example: a = [1,2,3,4]
    | AssignInteger String AExpr
    | Ifelse BExpr Program (Maybe Program)
    | Whiledo BExpr Program
    | Dowhile Program BExpr
    | Skip
    deriving Show
{--
instance Show Com where
    show (DeclareBoolean t (Just v)) = "Declare boolean " ++ show t ++ " = " ++ show v
    show (DeclareBoolean t Nothing) = "Declare boolean " ++ show t ++ " = nothing"
    show (DeclareInteger t (Just v)) = "Declare integer " ++ show t ++ " = " ++ show v
    show (DeclareInteger t Nothing) = "Declare integer " ++ show t ++ " = nothing "
    show (DeclareArray t v Nothing) = "Declare empty array " ++ show t ++ " of size " ++ show v
    show (DeclareArray t v (Just el)) = "Declare array " ++ show t ++ " of size " ++ show v ++ " with elements " ++ show el
    show (AssignBoolean s v) = "Assign " ++ show v ++ " to boolean " ++ show s
    show (AssignInteger s v) = "Assign " ++ show v ++ " to integer " ++ show s
    show (AssignArrayPosition a p v) = "Assign " ++ show v ++ " to position " ++ show p ++ " of array " ++ a
    show (AssignWholeArray n v) = "Assign " ++ show v ++ " to array "  ++ n
    show (Ifelse b x y) = "Command: If " ++ show b ++ " then " ++ show x ++ " else " ++ show y
    show (Whiledo b x) = "Command: While " ++ show b ++ " do " ++ show x
    show (Skip) = "Skip "
-}
type Program = [Com]
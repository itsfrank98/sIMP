module Grammar where

data Values =
    Integer Int
    | Boolean Bool
    | Array [Int]

instance Show Values where
    show (Integer a) = "Integer " ++ show a
    show (Boolean b) = "Boolean " ++ show b
    show (Array ar) = "Array " ++ show ar

--Arithmetic operations (sum, div, prod, power). 
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
{--
instance Show AExpr where
    show (Const a) = show a
    show (Ar n s) = "Array " ++ show n ++ "of size " ++ show s
    show (ArithmeticIdentifier i) = show i
    show (Add a1 a2) = "Sum: [" ++ show a1 ++ " + " ++ show a2 ++ "]"
    show (Diff a1 a2) = "Difference: [ " ++ show a1 ++ " - " ++ show a2 ++ "]"
    show (Div a1 a2) = "Division: [ " ++ show a1 ++ " / " ++ show a2 ++ "]"
    show (Prod a1 a2) = "Product: [ " ++ show a1 ++ " * " ++ show a2 ++ "]"
    show (Power a1 a2) = "[" ++ show a1 ++ " power " ++ show a2 ++ "]"
-}
data BExp =
    BVal Bool
    | IdentifierBool String
    | And BExp BExp
    | Or BExp BExp
    | Not BExp
    | Lt AExpr AExpr
    | Gt AExpr AExpr
    | Eq AExpr AExpr
    | Different AExpr AExpr
    | Lte AExpr AExpr
    | Gte AExpr AExpr
    deriving Show
{--
instance Show BExp where
    show (BVal b) = "{" ++ show b ++ "}"
    show (IdentifierBool i) = "Bool " ++ show i
    show (And a b) = "{" ++ show a ++ " and " ++ show b ++ "}"
    show (Or a b) = "{" ++ show a ++ " or " ++ show b ++ "}"
    show (Not a) = "not {" ++ show a ++ "}"
    show (Lt a b) = show a ++ " lt " ++ show b
    show (Gt a b) = show a ++ " gt " ++ show b
    show (Eq a b) = show a ++ " equals " ++ show b
    show (Different a b) = show a ++ " different " ++ show b
    show (Lte a b) = show a ++ " lte " ++ show b
    show (Gte a b) = show a ++ " gte " ++ show b
-}
data Com = DeclareBoolean String (Maybe BExp)
    | DeclareInteger String (Maybe AExpr)
    | DeclareArray String AExpr (Maybe AExpr)
    | AssignBoolean String BExp
    | AssignArrayPosition String AExpr AExpr --Assign a value to a specific array cell. Example: array[4] = 5
    | AssignWholeArray String AExpr --Assign whole array to a pre-declared variable. Example: a = [1,2,3,4]
    | AssignInteger String AExpr
    | Ifelse BExp Program (Maybe Program)
    | Whiledo BExp Program
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
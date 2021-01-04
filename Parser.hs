module Parser where
import Grammar(AExpr(..), BExpr(..), Com(..), Values(..))
import Utils(isDigit, isUpper, isLower, isLetter, isAlphaNum, isSpace)

newtype Parser a = P{
    runParser :: String -> Maybe(a, String)
}

parse :: String -> ([Com], String)
parse input = case runParser(programP) input of
    Nothing -> ([], "")
    Just(b, rest) -> (b, rest)

getParsedCommands :: ([Com], String) -> [Com]
getParsedCommands (c, _) = c

parseFailed :: ([Com], String) -> Maybe (String)
parseFailed (com, rest) =
    if (length rest == 0) then Nothing else (Just rest) 

--fmap is like a "penetration operation" because it takes a function.
--fmap :: (a -> b) -> f a -> f b means that fmap takes a function from
-- a to b and a functor defined on a. Then it allows to take the function
-- and inject it into the functor. As result you obtain the result of 
-- the first function wrapped into the same functor
instance Functor Parser where
    fmap f (P p) = P(\input -> case p input of
        Nothing -> Nothing
        Just(x, input') -> Just(f x, input'))

--The applicative allows to chain more parsers. We want parsers to be 
--executed in chain. Each parser will work on the output given by the 
--previous one. If a parser gives Nothing, the computation stops. So for each couple of parsers,
--the first will return a function and the second will run that function on the value it contains
instance Applicative Parser where
    pure f = P((\input -> Just(f, input)))
    --The first parser contains a function, the second contains its argument
    P(p1) <*> P(p2) = P(\input -> case p1 input of
        Nothing -> Nothing
        --I just unwrapped the function i will need to apply
        Just(f, input') -> case p2 input' of
            Nothing -> Nothing
            Just(x, input'') -> Just(f x, input'') 
            -- Here I also unwrap the argument of the function and at the end I return f x
        )

instance Monad Parser where
    (P p) >>= f = P(\input -> case p input of
        Nothing -> Nothing
        Just(v, other) -> case f v of
                    (P fv) -> fv other)

-- Since we can't import Control.Applicative, we reproduce it here
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many x

--Definition of the Alternative for Maybe
instance Alternative Maybe where
    empty = Nothing
    Nothing <|> foo = foo
    (Just x) <|> _ = Just x

--The alternative concatenates two parsers and returns the first non empty one. Since our parser returna a maybe, we take advantage of the fact that the maybe
-- is already an alternative, and use the <|> operator to concatenate the two unwrapped parsers that run the same input
instance Alternative Parser where
    empty = P(\input -> Nothing)
    P(p1) <|> P(p2) = P(\input -> p1 input <|> p2 input)

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chain` op = do a <- p; rest a
    where
        rest a = (do 
            f <- op
            b <- p
            rest (f a b))
            <|> return a


--Parser that fails if the input string is empty, otherwise returns a parser that has parsed the first char
item :: Parser Char
item = P( \input -> case input of
    [] -> Nothing
    (x: xs) -> Just(x,xs))


-- Checks if a char respects some predicate p
sat :: (Char -> Bool ) -> Parser Char
sat p = 
    do 
        x <- item
        if p x then return x else empty

--Parser of a digit
digitP :: Parser Char
digitP = sat isDigit

--Parser of uppercase letters
upperP :: Parser Char
upperP = sat isUpper

--Parser of lowercase letters
lowerP :: Parser Char
lowerP = sat isUpper

--Parser of a letter
letterP :: Parser Char
letterP = sat isLetter

--Parser of an alphanumeric character
alphaNumP :: Parser Char
alphaNumP = sat isAlphaNum

-- Parser of spaces
spacesP :: Parser ()
spacesP =
    do
        x <- many (sat isSpace)
        return ()

-- Parser of a single char
charP :: Char -> Parser Char
charP x = sat (== x)

-- Parser of a string
stringP :: String -> Parser String
stringP input = sequenceA (map charP input)

-- Parser of a single identifier
singleIdentifierP :: Parser String
singleIdentifierP =
    do
        x <- letterP
        xs <- many alphaNumP
        return (x : xs)

-- Parser of a single natural number
singleNatP :: Parser Int
singleNatP =
    do
        x <- some digitP
        return (read x)

-- Parser of a single integer
singleIntegerP :: Parser Int
singleIntegerP =
    do
        charP '-'
        n <- naturalNumP
        return (-n)
    <|> naturalNumP

-- Parser of a single token. This allows to ignore mutiple whitespaces around tokens
tokenP :: Parser a -> Parser a
tokenP p =
    do
        spacesP
        v <- p
        spacesP
        return v

--Parser for an identifier that ignores whitespaces
identifierP :: Parser String
identifierP = tokenP singleIdentifierP

naturalNumP :: Parser Int
naturalNumP = tokenP singleNatP

--Parser for an integer that ignores whitespaces
integerP :: Parser Int
integerP = tokenP singleIntegerP

-- Parser for an array of integers
arrayP :: Parser [Int]
arrayP = do
    symbolP "["
    n <- integerP
    ns <- many (
        do
            symbolP ","
            integerP)
    symbolP "]"
    return (n:ns)

symbolP :: String -> Parser String
symbolP xs = tokenP (stringP xs)

aExprP :: Parser AExpr
aExprP = arithmeticTerm `chain` op where
    op = do
        symbolP "+"
        return Add
        <|>
        do
            symbolP "-"
            return Diff

arithmeticTerm :: Parser AExpr
arithmeticTerm = arithmeticFactor `chain` op where
    op = do
        symbolP "/"
        return Div
        <|>
        do
            symbolP "*"
            return Prod
        <|>
        do
            symbolP "^"
            return Power

arithmeticFactor :: Parser AExpr
arithmeticFactor = do
    symbolP "("
    expr <- aExprP
    symbolP ")"
    return expr
    <|>
    do 
        (Const <$> integerP)
    <|>
    do
        (ConstArr <$> arrayP)
    <|>
    do
        symbolP "top"
        symbolP "("
        name <- identifierP
        symbolP ")"
        return (Top name)
    <|>
    do
        i <- identifierP
        do
            symbolP "["
            n <- aExprP
            symbolP "]"
            return (Ar i n)
            <|>
            return (ArithmeticIdentifier i)

-- Boolean expressions Parser
bExprP :: Parser  BExpr
bExprP = booleanTerm `chain` op where
    op = do
        symbolP "or"
        return Or

booleanTerm :: Parser BExpr
booleanTerm = booleanFactor `chain` op where
    op = do
        symbolP "and"
        return And

booleanFactor :: Parser BExpr
booleanFactor = do
    symbolP "True"
    return (BVal True)
    <|>
    do
        symbolP "False"
        return (BVal False)
    <|>
    do
        symbolP "empty"
        symbolP "("
        name <- identifierP
        symbolP ")"
        return (Empty name)
    <|>
    do
        symbolP "("
        bExpression <- bExprP
        symbolP ")"
        return bExpression
    <|>
    do
        symbolP "!"
        (Not <$> bExprP)
    <|>
    do
        a <- aExprP
        do
            symbolP "<"
            (Lt a <$> aExprP)
            <|>
            do
                symbolP ">"
                (Gt a <$> aExprP)
            <|>
            do
                symbolP "=="
                (Eq a <$> aExprP)
            <|>
            do
                symbolP "<>"
                (Different a <$> aExprP)
            <|>
            do
                symbolP "<="
                (Lte a <$> aExprP)
            <|>
            do
                symbolP ">="
                (Gte a <$> aExprP)
    <|>
    do
        i <- identifierP
        return (BooleanIdentifier i)


-- Parser of commands
commandP :: Parser Com
commandP = 
    integerDeclare 
    <|>
    booleanDeclare
    <|>
    arrayDeclare
    <|>
    stackDeclare
    <|>
    integerAssign
    <|>
    booleanAssign
    <|>
    arrayAssignPosition
    <|>
    arrayAssignWhole
    <|>
    push 
    <|>
    pop
    <|>
    ifThenElse
    <|>
    while
    <|>
    dowhile
    <|>
    skip

programP :: Parser [Com]
programP = do many commandP

integerDeclare :: Parser Com
integerDeclare = do
    symbolP "int"
    name <- identifierP
    do
        symbolP "="
        val <- aExprP
        symbolP ";"
        return (DeclareInteger name (Just val))
        <|>
        do
            symbolP ";"
            return (DeclareInteger name Nothing)

booleanDeclare :: Parser Com
booleanDeclare = do
    symbolP "bool"
    name <- identifierP
    do
        symbolP "="
        val <- bExprP
        symbolP ";"
        return (DeclareBoolean name (Just val))
        <|>
        do
            symbolP ";"
            return (DeclareBoolean name Nothing)

arrayDeclare :: Parser Com
arrayDeclare = do
    symbolP "array"
    symbolP "["
    size <- aExprP
    symbolP "]"
    name <- identifierP
    do
        symbolP "="
        val <- aExprP
        symbolP ";"
        return (DeclareArray name size (Just val))
        <|>
        do
            symbolP ";"
            return (DeclareArray name size Nothing)

stackDeclare :: Parser Com
stackDeclare = do
    symbolP "stack"
    name <- identifierP
    symbolP ";"
    return (DeclareStack name)

booleanAssign :: Parser Com
booleanAssign = do
    name <- identifierP
    symbolP "="
    val <- bExprP
    symbolP ";"
    return (AssignBoolean name val)

integerAssign :: Parser Com
integerAssign = do
    name <- identifierP
    symbolP "="
    val <- aExprP
    symbolP ";"
    return (AssignInteger name val)

push :: Parser Com
push = do
    symbolP "push"
    symbolP "("
    name <- identifierP
    symbolP ","
    val <- aExprP
    symbolP ")"
    symbolP ";"
    return (Push name val)

pop :: Parser Com
pop = do
    symbolP "pop"
    symbolP "("
    name <- identifierP
    symbolP ")"
    symbolP ";"
    return (Pop name)

arrayAssignPosition :: Parser Com
arrayAssignPosition = do
    name <- identifierP
    symbolP "["
    position <- aExprP
    symbolP "]"
    symbolP "="
    val <- aExprP
    symbolP ";"
    return (AssignArrayPosition name position val)

arrayAssignWhole :: Parser Com
arrayAssignWhole = do
    name <- identifierP
    symbolP "="
    val <- aExprP
    symbolP ";"
    return (AssignWholeArray name val)

ifThenElse :: Parser Com
ifThenElse = do
    symbolP "if"
    symbolP "("
    condition <- bExprP
    symbolP ")"
    symbolP "{"
    prog <- programP
    symbolP "}"
    do
        symbolP "else"
        symbolP "{"
        elseProg <- programP
        symbolP "}"
        return (Ifelse condition prog (Just elseProg))
        <|>
        do
            return (Ifelse condition prog Nothing)

while :: Parser Com
while = do
    symbolP "while"
    symbolP "("
    condition <- bExprP
    symbolP ")"
    symbolP "{"
    prog <- programP
    symbolP "}"
    return (Whiledo condition prog)

dowhile :: Parser Com
dowhile = do
    symbolP "do"
    symbolP "{"
    prog <- programP
    symbolP "}"
    symbolP "while"
    symbolP "("
    condition <- bExprP
    symbolP ")"
    symbolP ";"
    return (Dowhile prog condition)

skip :: Parser Com
skip = do
    symbolP "skip"
    symbolP ";"
    return Skip

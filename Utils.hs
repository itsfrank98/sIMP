module Utils where
import Grammar(AExpr(..))

isDigit :: Char -> Bool
isDigit a = a `elem` ['0'..'9']

isUpper :: Char -> Bool
isUpper a = a `elem` ['A'..'Z']

isLower :: Char -> Bool
isLower a = a `elem` ['a'..'z']

isLetter :: Char -> Bool
isLetter a = isUpper a || isLower a

isAlphaNum :: Char -> Bool
isAlphaNum a = isLetter a || isLower a

isSpace :: Char -> Bool
isSpace x = x `elem` [' ', '\n']

readArray :: [Int] -> Int -> Int
readArray [] _ = error "Index out of bound"
readArray (x:xs) pos = if (pos < 0)
    then error "Illegal index"
    else
        if(pos == 0)
            then x
            else readArray xs (pos - 1)

-- From left to write: initial array, position, value to write, updated array
writeArray :: [Int] -> Int -> Int -> (Maybe [Int])
writeArray [] _ _ = Nothing
writeArray (x:xs) pos val = if(pos == 0)
    then Just(val:xs)
    else (([x]++) <$> (writeArray xs (pos-1) val))

-- Creates an array full of zeros of the given size
createEmptyArray :: Int -> [Int]
createEmptyArray size = if (size < 1)
    then error "Size error"
    else
        if (size == 1)
            then [0]
            else [0] ++ (createEmptyArray (size -1))

-- Given a position, removes from the list the element in that position
removeElem :: [a] -> Int -> [a]
removeElem [] _ = error "Index out of bound"
removeElem (x : xs) pos = if (pos < 0)
    then error "Negative index"
    else
        if (pos == 0)
            then xs
            else ([x] ++ removeElem xs (pos - 1))

power:: Int -> Int -> Int
power 0 0 = 0
power _ 0 = 1
power a b = a * (power a (b-1))

ratio :: Int -> Int -> Int
ratio 0 _ = 0
ratio a b = a `div` b
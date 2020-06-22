module Main where

main :: IO ()
main = putStrLn "Hello, world!!"

-- | Will select first value of the tuple.
first  :: (a, b, c) -> a
first  (a, _, _) = a

-- | Will select second value of the tuple.
second :: (a, b, c) -> b
second (_, b, _) = b

-- | Will select third value of the tuple.
third  :: (a, b, c) -> c
third  (_, _, c) = c

-- | Function 'insertCol' will get 2D array of chars and will write down provided word into selected column at given position.
insertCol :: [[Char]]   -- ^ Input (2D array).
            -> [Char]   -- ^ Word to insert.
            -> Int      -- ^ Column, to which the word sohuld be inserted.
            -> Int      -- ^ Position in column, at which the word should be inserted.
            -> [[Char]] -- ^ Output.
insertCol i w col p = insertCol' i w col 0 p

-- | Helper function for 'insertCol', with one before the last argument being row counter (counting from 0).
insertCol' :: [[Char]] -> [Char] -> Int -> Int -> Int -> [[Char]]
insertCol' i [] _ _ _ = i
insertCol' (i:is) (w:ws) col c p
    | p == c    = (replaceItem i [w] col) : insertCol' is ws col c p
    | otherwise = i : (insertCol' is (w:ws) col (c + 1) p)

-- | Function 'insertRow' will get 2D array of chars and will write down provided word into selected row at given position.
insertRow :: [[Char]]   -- ^ Input (2D array).
            -> [Char]   -- ^ Word, which should be inserted.
            -> Int      -- ^ Row, to which the word should be inserted.
            -> Int      -- ^ Position, at which the word should be inserted.
            -> [[Char]] -- ^ Output.
insertRow i w r p = insertRow' i w r 0 p

-- | Helper function for 'insertRow', with one before the last argument being row counter (counting from 0).
insertRow' :: [[Char]] -> [Char] -> Int -> Int -> Int -> [[Char]]
insertRow' (i:is) w r c p
    | r == c    = (replaceItem i w p) : is
    | otherwise = i : insertRow' is w r (c + 1) p

-- | Function will replace part of given array at given position.
replaceItem :: (Ord a) => [a] -- ^ Input array.
                -> [a]        -- ^ Replacement array.
                -> Int        -- ^ Where to replace.
                -> [a]        -- ^ Output.
replaceItem a i b
    | b >= (length a) = a
    | otherwise       = replaceItem' a i b 0

replaceItem' :: (Ord a) => [a] -> [a] -> Int -> Int -> [a]
replaceItem' a [] _ _ = a
replaceItem' (a:as) (i:is) b c
    | b == c    = i : (replaceItem' as is b c)
    | otherwise = a : (replaceItem' as (i:is) b (c + 1))

data RowSpace = WordSpace (Int, Int) (Int, Int) Bool

-- | Function for getting length of space with given wordspace.
--
-- Posivtive values means rows, negative values means columns.
spaceLen :: RowSpace -> Int
spaceLen (WordSpace (a1, a2) (b1, b2) _) =  (b1 - a1) + (-1 * (b2 - a2))
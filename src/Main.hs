module Main where

main :: IO ()
main = putStrLn "Hello, world!!"

-- | Will select first value of the tuple
first  :: (a, b, c) -> a
first  (a, _, _) = a

-- | Will select second value of the tuple
second :: (a, b, c) -> b
second (_, b, _) = b

-- | Will select third value of the tuple
third  :: (a, b, c) -> c
third  (_, _, c) = c

-- | Function 'insertRow' will get 2D array of chars and will write down provided word into selected row at given position.
insertRow :: [[Char]]   -- ^ Input (2D array)
            -> [Char]   -- ^ Word, which should be inserted
            -> Int      -- ^ Row, to which the word should be inserted
            -> Int      -- ^ Position, at which the word should be inserted
            -> [[Char]] -- ^ Output
insertRow i w r p = insertRow' i w r 0 p

-- | Helper function for inserting row, with last argument being row counter.
insertRow' :: [[Char]] -> [Char] -> Int -> Int -> Int -> [[Char]]
insertRow' (i:is) w r c p
    | r == c    = (replaceItem i w p) : is
    | otherwise = i : insertRow' is w r (c + 1) p

replaceItem :: (Ord a) => [a] -> [a] -> Int -> [a]
replaceItem a i b
    | b >= (length a) = a
    | otherwise       = replaceItem' a i b 0

replaceItem' :: [a] -> [a] -> Int -> Int -> [a]
replaceItem' a [] _ _ = a
replaceItem' (a:as) (i:is) b c
    | b == c    = i : (replaceItem' as is b c)
    | otherwise = a : (replaceItem' as (i:is) b (c + 1))
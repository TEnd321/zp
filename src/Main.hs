module Main where

main :: IO ()
main = putStrLn "Hello, world!!"

-- | Will select first value of the tuple
first :: (a, b, c) -> a
first  (a, _, _) = a
-- | Will select second value of the tuple
second :: (a, b, c) -> b
second (_, a, _) = a
-- | Will select third value of the tuple
third :: (a, b, c) -> c
third  (_, _, a) = a

-- | Function 'insertRow' will get 2D array of chars and will write down provided word into selected row at given position.
insertRow :: [[Char]]   -- ^ Input (2D array)
            -> [Char]   -- ^ Word, which should be inserted
            -> Int      -- ^ Row, to which the word should be inserted
            -> Int      -- ^ Position, at which the word should be inserted
            -> [[Char]] -- ^ Output
insertRow a b c d = undefined
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
insertCol i [] _ _ = i
insertCol (i:is) (w:ws) col p
    | p == 0    = (replaceItem i [w] col) : insertCol is ws col p
    | otherwise = i : (insertCol is (w:ws) col (p - 1))

-- | Function 'insertRow' will get 2D array of chars and will write down provided word into selected row at given position.
insertRow :: [[Char]]   -- ^ Input (2D array).
            -> [Char]   -- ^ Word, which should be inserted.
            -> Int      -- ^ Row, to which the word should be inserted.
            -> Int      -- ^ Position, at which the word should be inserted.
            -> [[Char]] -- ^ Output.
insertRow (i:is) w r p
    | r == 0    = (replaceItem i w p) : is
    | otherwise = i : insertRow is w (r - 1) p

-- | Function will replace part of given array at given position.
replaceItem :: (Ord a) => [a] -- ^ Input array.
                -> [a]        -- ^ Replacement array.
                -> Int        -- ^ Where to replace.
                -> [a]        -- ^ Output.
replaceItem a [] _ = a
replaceItem (a:as) (i:is) b
    | b == 0    = i : (replaceItem as is b)
    | otherwise = a : replaceItem as (i:is) (b - 1)

data RowSpace = WordSpace (Int, Int) (Int, Int) Bool

-- | Function for getting length of space with given wordspace.
--
-- Posivtive values means rows, negative values means columns.
spaceLen :: RowSpace -> Int
spaceLen (WordSpace (a1, a2) (b1, b2) _) =  (b1 - a1) + (-1 * (b2 - a2))

-- | Difference list implementation.
--
-- Difference list have both append and head operations with O(1) complexity and therefore are perfect replacement for queue.
newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

instance (Show a) => Show (DiffList a) where
    show (DiffList f) = show (f [])

instance Semigroup (DiffList a) where
    DiffList a <> DiffList b = DiffList (\dl -> a . b $ dl)

instance Monoid (DiffList a) where
    mempty = DiffList (\dl -> [] ++ dl)

toDiffList :: [a] -> DiffList a
toDiffList a = DiffList (a ++)

dlToList :: DiffList a -> [a] -> [a]
dlToList (DiffList a) b = a b

dlAppend :: a -> DiffList a -> DiffList a
dlAppend a dl = dl <> (DiffList ([a] ++))

dlHead :: DiffList a -> a
dlHead dl = head (dlToList dl [])

dlPop :: (Eq a) => DiffList a -> Maybe (DiffList a)
dlPop dl
    | dl == mempty = Nothing
    | otherwise    = Just (toDiffList (tail (dlToList dl []))) 

instance (Eq a) => Eq (DiffList a) where 
    a == b = (dlToList a []) == (dlToList b [])
    a /= b = (dlToList a []) /= (dlToList b [])
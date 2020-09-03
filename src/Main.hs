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
insertCol [] _ _ _ = []
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
insertRow [] _ _ _ = []
insertRow (i:is) w r p
    | r == 0    = (replaceItem i w p) : is
    | otherwise = i : insertRow is w (r - 1) p

-- | Function will replace part of given array at given position.
replaceItem :: (Ord a) => [a] -- ^ Input array.
                -> [a]        -- ^ Replacement array.
                -> Int        -- ^ Where to replace.
                -> [a]        -- ^ Output.
replaceItem [] _ _ = []
replaceItem a [] _ = a
replaceItem (a:as) (i:is) b
    | b == 0    = i : (replaceItem as is b)
    | otherwise = a : replaceItem as (i:is) (b - 1)

data Space = Space (Int, Int) (Int, Int)
    deriving (Show, Eq)

-- | Function for getting length of space with given wordspace.
--
-- Positive values means rows, negative values means columns.
spaceLen :: Space -> Int
spaceLen (Space (a1, a2) (b1, b2)) =  (b1 - a1) + (-1 * (b2 - a2))

-- | Difference list implementation.
--
-- Difference lists have both append and prepend operations with O(1) complexity and therefore are perfect replacement for queue.
newtype DiffList a = DiffList {
    getDiffList :: [a] -> [a]
    }

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

-- | Will return the head of the difference list
dlHead :: DiffList a -> a
dlHead dl = head (dlToList dl [])

-- | Will pop the head of the difference list
dlPop :: (Eq a) => DiffList a -> Maybe (DiffList a)
dlPop dl
    | dl == mempty = Nothing
    | otherwise    = Just (toDiffList (tail (dlToList dl []))) 

instance (Eq a) => Eq (DiffList a) where 
    a == b = (dlToList a []) == (dlToList b [])
    a /= b = (dlToList a []) /= (dlToList b [])

-- | Will roll array one step to the left.
-- >>> rollArray [1,2,3]
-- [2,3,1]
--
rollArray :: [a] -> [a]
rollArray [] = []
rollArray (a:as) = as ++ [a]
--
--bfsEnqueue :: (DiffList a)     -- ^ Queue
--        -> [(Int, [String])]   -- ^ Words
--        -> [(Int, [Space])]    -- ^ Spaces
--        -> [[Char]]            -- ^ Crossword puzzle
--bfsEnqueue = undefined
--
--bfsDequeue :: DiffList ([(Int, [String])], [(Int, [Space])], [[Char]]) -- ^ Queue, which enters BFS level
--        -> DiffList ([(Int, [String])], [(Int, [Space])], [[Char]])    -- ^ Queue, which leaves BFS level
--bfsDequeue (DiffList (((wl,w):ws), ((l, r:rs):ss), cp)) 
--    | (spaceLen r) > 0 = (insertRow w )

-- | Funcion will sort given list of words by their length, descending.
-- >>> sort ["aaa", "zzzz", "tt"]
-- ["zzzz","aaa","tt"]
--
sort :: [String] -> [String]
sort []     = []
sort (p:ps) = (sort earlier) ++ [p] ++ (sort later)
    where earlier = filter (wordFilter   (< ) p) ps
          later   = filter (wordFilter   (>=) p) ps

wordFilter :: (Int -> Int -> Bool) -> String -> String -> Bool
wordFilter f w1 w2 
    | (length w1) `f` (length w2) = True
    | otherwise                   = False

-- | Following method will create groups of words with equal length, assuming, that the words are already sorted properly.
--   Leaving out empty lists where no words of given length were found.
-- >>> wordsSeparate ["hello", "world", "how", "are", "you", "?"]
-- [["hello","world"],[],["how","are","you"],[],["?"]]
--
wordsSeparate :: [String] -> [[String]]
wordsSeparate [] = []
wordsSeparate (s:ss) = let l = (length s) in wordSeparate' (s:ss) l

-- | Helper function for 'wordsSeparate'
wordSeparate' :: [String] -> Int -> [[String]]
wordSeparate' [] _ = []
wordSeparate' _ 0  = []
wordSeparate' a l = let res = (separateLength a l) in res:(wordSeparate' (drop (length res) a) (l - 1))

-- >>> separateLength ["ahoj", "svet", "ako", "mas", "sa", "ty", "?", "!"] 4
-- ["ahoj","svet"]
--
separateLength :: [String] -> Int -> [String]
separateLength [] _ = []
separateLength (a:as) l 
    | (length a) == l = (a:(separateLength as l))
    | (length a) >  l = separateLength (as) l
    | otherwise = []
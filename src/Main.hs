module Main where
    
import System.IO
import Data.List
import Text.Read
import Options.Applicative

import Args

main :: IO ()
main = do
    options <- execParser opts
    handlePuzzle <- openFile (puzzleInput options) ReadMode
    sizes <- hGetLine handlePuzzle
    spaces <- hGetContents handlePuzzle
    handleWords <- openFile (wordsInput options) ReadMode
    inwords <- hGetContents handleWords
    putStr (buildPuzzle sizes spaces inwords options)

-- | Given all the files, function will process the input using 'lines' and will return all the puzzles in printable format.
--
-- All files are read only once. According to entry parameters, spaces which are 1x1, are (not) omitted.
--
-- \(O(n^2 \times m)^2\) where \(n\) is biggest number of words of given length and \(m\) is number of unique lengths
buildPuzzle :: String -- ^ Dimensions of the puzzle.
            -> String -- ^ 2D Array of all given spaces.
            -> String -- ^ List of words.
           -> Options -- ^ Given options from command line.
            -> String -- ^ Output puzzles in readable format.
buildPuzzle dimensions ispaces iwords options = unlines ( map unlines ( printPuzzle ( removeDuplicities (startSolvingPuzzle puzzle sWords spaces))))
    where
        dimList = lines (lineify dimensions)
        (row, col) = (readMaybe (head dimList), readMaybe (head . reverse $ dimList))
        emptyPuzzle = createPuzzleState row col
        spacesUnsorted = importSpaces ispaces
        puzzle = if (excludeOnes options)
                    then changeSpaces emptyPuzzle (length spacesUnsorted)
                    else changeSpaces emptyPuzzle (length (filter ((> 1) . abs . spaceLen) spacesUnsorted))
        spaces = removeOnesDuplicities (sortSpaces spacesUnsorted (not . excludeOnes $ options))
        sWords = loadWords iwords (spaceLen (head . head $ spaces))

-- | Will remove all duplicate 1x1 spaces.
--
-- Complexity: \(O(n + m^2)\), where \(n\) is the number of all different lengths of all spaces, \(m\) is the number of all 1x1 spaces.
--             \(O(1)\), if 1x1 spaces are disabled.
removeOnesDuplicities :: [[Space]] -- ^ All spaces present in puzzle.
                      -> [[Space]] -- ^ Output, duplicit 1x1 spaces are removed.
removeOnesDuplicities [] = []
removeOnesDuplicities (s:spaces) = if (s /= []) && ((spaceLen (head s)) == 1) then [removeDuplicities s] else s:removeOnesDuplicities spaces

-- | Will remove all occurences of given item from list. Selected item is prepended and therefore one instance of given item is kept.
--
-- Complexity: \(O(n^2)\), where n is the number of items in given list. For each item, new list without that item is created. Complexity of that is \(O(n)\).
removeDuplicities :: (Eq a) => [a] -- ^ Input list
                            -> [a] -- ^ Output list, multiple occurencies removed.
removeDuplicities [] = []
removeDuplicities (s:spaces) = s:(removeDuplicities (removeAllOfItem spaces s))

-- | Will remove all occurences of given item in given array.
--
-- Complexity: \(O(n)\), where n is number of elements in given list, as function will traverse whole list once, and will copy only items, which wont be deleted.
removeAllOfItem :: (Eq a) => [a] -- ^ Input list.
                           -> a  -- ^ Item to be removed.
                          -> [a] -- ^ List with all occurencies of given item removed.
removeAllOfItem [] _ = []
removeAllOfItem (i:items) item
    | i == item = removeAllOfItem items item
    | otherwise = i:(removeAllOfItem items item)

-- | Replace all the spaces with "\\n" character. Such string could then be used with 'lines' function.
--
-- Complexity:
-- \(O(n)\); whole string is traversed once.
lineify :: String -- ^ Input string; first space will be replaced by "\n"
        -> String -- ^ Output
lineify [] = []
lineify (s:string)
    | s /= ' ' = s:(lineify string)
    | otherwise = "\n" ++ string

-- | Given string as-is from IO, function will use 'lines' to parse these lines, sort given words and separate them accordingly by their length.
--   Only words short enough are preserved, thus the 'filter' is used.
--
-- \(O(n^3)\); \(n\) is the number of words divided by "\\n"
loadWords :: String -- ^ Input string as read by 'hGetContents'
             -> Int -- ^ Maximum length of words
      -> [[String]] -- ^ All words separated by their lengths accordingly into sublists.
loadWords []  _ = []
loadWords inp i = let iwords = lines inp in listsSeparate (qsort (filter ((<= i) . length) iwords) length) length

-- | Given the list of all spaces, and whether to ignore 1x1 spaces or not, function will sort and separate spaces of equal length.
-- 
-- \(O(n^2)\); \(n\) is the length of input array
sortSpaces :: [Space] -- ^ Input list of spaces.
              -> Bool -- ^ Include 1x1 spaces?
         -> [[Space]] -- ^ Spaces separated by their lengths into sublists.
sortSpaces spaces ones =
    if ones then listsSeparate (qsort spaces (abs . spaceLen)) (abs . spaceLen)
            else listsSeparate (qsort (filter ((> 1) . abs . spaceLen) spaces) (abs . spaceLen)) (abs . spaceLen)

-- | Given string of spaces, function use 'lines' to parse the input and will call 'importSpaces\''.
importSpaces:: String -- ^ String of input spaces.
           -> [Space] -- ^ List of all spaces found.
importSpaces a = let allLines = lines a in importSpaces' allLines allLines 0

-- | Given the list of spaces in string representation, function will parse given lines and generate spaces accordingly.
--
-- \(O(n \times m)\); where \(n\) is number of rows and \(m\) is number of columns.
importSpaces' :: [String] -- ^ Unparsed spaces
              -> [String] -- ^ Backup; used by 'spacesFromColumns'
                   -> Int -- ^ Which row is currently being parsed. Should be set by caller to 0.
               -> [Space] -- ^ Parsed spaces.
importSpaces' []     backup _   = spacesFromColumns (transpose backup) 0
importSpaces' (a:as) backup row = (parseLine a row 0 0 'd') ++ (importSpaces' as backup (row + 1))

-- | Given the list of spaces in string representation, function will parse given lines and generate spaces accordingly.
--
-- \(O(n \times m)\); where \(n\) is number of columns and \(m\) is number of rows.
spacesFromColumns :: [String] -- ^ Unparsed spaces.
                       -> Int -- ^ Which column is currently being parsed. Should be set by caller to 0.
                   -> [Space] -- ^ Parsed spaces.
spacesFromColumns [] _           = []
spacesFromColumns (p:puzzle) col = (parseColumn p col 0 0 'r') ++ (spacesFromColumns puzzle (col + 1))

-- | From list of 'GraphNode' is taken puzzle 2D array and exported.
--
-- \(O(n)\); where \(n\) is number of nodes.
printPuzzle :: [GraphNode] -> [[String]]
printPuzzle []        = []
printPuzzle (n:nodes) = p:(printPuzzle nodes)
    where
        (Node (Puzzle _ p) _ _) = n

-- | Function will parse given line, where 'n' and input 'Char' are both ignored regarding wether there is a wall or not. Any other char will be recognized as non-wall char.
--
-- \(O(n)\); where \(n\) is length of input string.
parseLine :: String -- ^ String to parse.
            -> Int -- ^ Which row is selected.
            -> Int -- ^ Start of the current space.
            -> Int -- ^ Number of free "spaces" that current space has yet.
            -> Char -- ^ Char to be ignored
            -> [Space]
parseLine []     row start steps _ = [Space (start, row) ((start + steps - 1), row)]
parseLine (s:st) row start steps ch
    | (s == 'n') || (s == ch) = parseLine st row start (steps + 1) ch
    | otherwise = (Space (start, row) ((start + steps), row)):(parseLine st row (start + steps + 1) 0 ch)

-- | Function will parse given line, where 'n' and input 'Char' are both ignored regarding wether there is a wall or not. Any other char will be recognized as non-wall char.
--
-- \(O(n)\); where \(n\) is length of input string.
parseColumn :: String -- ^ String to parse.
            -> Int -- ^ Which row is selected.
            -> Int -- ^ Start of the current space.
            -> Int -- ^ Number of free "spaces" that current space has yet.
            -> Char -- ^ Char to be ignored
            -> [Space]
parseColumn []     col start steps _ = [Space (col, start) (col, (start + steps - 1))]
parseColumn (s:st) col start steps ch
    | (s == 'n') || (s == ch) = parseColumn st col start (steps + 1) ch
    | otherwise = (Space (col, start) (col, (start + steps))):(parseColumn st col (start + steps + 1) 0 ch)

-- | Given initial 'PuzzleState', function will begin solving the puzzle using 'generateLevel'.
startSolvingPuzzle :: Maybe (PuzzleState) -- ^ Initial puzzle state. If value equals 'Nothing', empty list is returned.
                            -> [[String]] -- ^ List of available words.
                             -> [[Space]] -- ^ List of available spaces.
                           -> [GraphNode] -- ^ List of puzzles, where all spaces are filled.
startSolvingPuzzle Nothing _ _ = []
startSolvingPuzzle (Just (Puzzle i pz)) (w:words) (s:spaces) = generateLevel firstNode len
    where 
        len = length w
        firstNode = Node (Puzzle i pz) (w:words) (s:spaces)

-- | Will create empty PuzzleState with no Spaces remaining.
--
-- No. of Spaces can be modified with 'changeSpaces'. If value in rows or columns was incorrect, will result in 'Nothing'
--
-- \(O(r \times c)\); \(r\) is number of rows and \(c\) is number of columns
createPuzzleState :: Maybe Int -- ^ Number of rows
                  -> Maybe Int -- ^ Number of columns
        -> Maybe (PuzzleState) -- ^ New 'PuzzleState'
createPuzzleState (Just 0) _ = Nothing
createPuzzleState _ (Just 0) = Nothing
createPuzzleState (Just rows) (Just cols) = Just (Puzzle 0 (replicate rows (replicate cols '.')))
createPuzzleState _ _ = Nothing

-- | Will change no. of Spaces in given puzzle.
--
-- \(O(1)\)
changeSpaces :: Maybe (PuzzleState) -- ^ Old 'PuzzleState'
                             -> Int -- ^ New space count
             -> Maybe (PuzzleState) -- ^ New 'PuzzleState'
changeSpaces Nothing _ = Nothing
changeSpaces (Just (Puzzle _ p)) new = Just (Puzzle new p)

-- | Function 'insertCol' will get 2D array of chars and will write down provided word into selected column at given position.
--
-- \(O(n)\), where \(n\) is the length of given list
insertCol :: [[Char]]   -- ^ Input (2D array).
            -> [Char]   -- ^ Word to insert.
            -> Int      -- ^ Column, to which the word sohuld be inserted.
            -> Int      -- ^ Position in column, at which the word should be inserted.
            -> [[Char]] -- ^ Output.
insertCol [] _ _ _ = []
insertCol input [] _ _ = input
insertCol (i:input) (w:word) col p
    | p == 0    = (replaceItem i [w] col) : insertCol input word col p
    | otherwise = i : (insertCol input (w:word) col (p - 1))

-- | Function 'insertRow' will get 2D array of chars and will write down provided word into selected row at given position.
--
-- \(O(n)\), where \(n\) is the length of given list
insertRow :: [[Char]]   -- ^ Input (2D array).
            -> [Char]   -- ^ Word, which should be inserted.
            -> Int      -- ^ Row, to which the word should be inserted.
            -> Int      -- ^ Position, at which the word should be inserted.
            -> [[Char]] -- ^ Output.
insertRow [] _ _ _ = []
insertRow (i:is) w r p
    | r == 0    = (replaceItem i w p) : is
    | otherwise = i : insertRow is w (r - 1) p

dummy :: [[Char]]
dummy = ["1234", "5678", "9abc", "def0"]

-- | Peek sublist from given 'PuzzleState' with provided 'Space'.
--
-- >>> peekWithSpace (Puzzle 0 ["1234", "5678", "9abc", "def0"]) (Space (0,0) (2,0))
-- "123"
--
-- \(O(n + m)\), where \(n\) is the number of rows is selected and \(m\) is the length of the word. If row is being selected.
--
-- \(O (n \times m)\) is '!!' operator and \(m\) is the length of the word. If column is being selected.
peekWithSpace :: PuzzleState -> Space -> String
peekWithSpace p s
    | (bCol == eCol) && (bRow == eRow) = [(a !! bRow) !! bCol]
    | otherwise = 
    if (len `div` (abs len)) == 1 
        then drop bCol . take (eCol + 1) $ (a !! bRow)
        else peekSubColumn (drop bRow a) (eRow - bRow + 1) bCol 
            where Puzzle i a = p; len = spaceLen s; Space (bCol, bRow) (eCol, eRow) = s

-- | Will peek subcolumn from given column, one piece after another.
--
-- \(O(n * m)\), where \(n\) is '!!' operator and \(m\) is the length of the word
peekSubColumn :: [[Char]] -- ^ Input
                -> Int -- ^ Count; length of word
                -> Int -- ^ Which column
                -> String -- ^ Output
peekSubColumn _ 0 _ = []
peekSubColumn (a:as) i c = (a !! c) : (peekSubColumn as (i - 1) c)

-- | Given 'Space' and 'PuzzleState', function will delegate inserting the word to either 'insertRow' or 'insertCol'.
insertForSpace ::  PuzzleState   -- ^ Input (2D array with counter of inserted words).
                -> [Char]   -- ^ Word, which should be inserted.
                -> Space    -- ^ Space where to insert.
                -> PuzzleState -- ^ Output.
insertForSpace p [] _ = p
insertForSpace p w (Space (fx, fy) (dx, dy))
    | (fx, fy) == (dx, dy) = Puzzle (l - 1) (insertRow i w fx fy)
    | otherwise =
    if (len `div` (abs len)) == 1 
        then Puzzle (l - 1) (insertRow i w fy fx)
        else Puzzle (l - 1) (insertCol i w fx fy)
            where len = spaceLen (Space (fx, fy) (dx, dy)); (Puzzle l i) = p

-- | Function will replace part of given array at given position.
--
-- \(O(n)\); where \(n\) is the length of the input list
replaceItem :: (Ord a) => [a] -- ^ Input array.
                       -> [a] -- ^ Replacement array.
                       -> Int -- ^ Where to replace.
                       -> [a] -- ^ Output.
replaceItem [] _ _ = []
replaceItem a [] _ = a
replaceItem (a:as) (i:is) b
    | b == 0    = i : (replaceItem as is b)
    | otherwise = a : replaceItem as (i:is) (b - 1)

-- | For representation of space, which will be filled during puzzle solving.
--
-- List of spaces is maintained during entire execution of the program, where unfilled spaces are still kept in the list.
-- Filled ones are removed from the list and word is filled into puzzle 2D array.
data Space = Space (Int, Int) (Int, Int)
    deriving (Show, Eq)

-- | Function for getting length of space with given wordspace.
--
-- Positive values means rows, negative values means columns.
--
-- \(O(1)\), only constant calculations are used.
spaceLen :: Space -> Int
spaceLen (Space (a1, a2) (b1, b2)) 
    | (a1 == b1) && (a2 == b2) = 1
    | (a1 == b1) && (a2 /= b2) = (-1) * (b2 - a2 + 1)
    | (a1 /= b1) && (a2 == b2) = (b1 - a1 + 1)
    | otherwise                = 0

-- | Will roll array one step to the left.
-- >>> rollArray [1,2,3]
-- [2,3,1]
--
-- \(O(n)\) where \(n\) is length of given list.
rollArray :: [a] -> [a]
rollArray [] = []
rollArray (a:as) = as ++ [a]

-- | Funcion will sort given list of words/spaces by their length, descending.
-- >>> sort ["aaa", "zzzz", "tt"] length
-- ["zzzz","aaa","tt"]
--
-- \(O(n^2) \times f \times log(n))\), where \(n\) is the length of given array and \(f\) is the comlexity of filtering function 
qsort ::   [a] -- ^ Input
 -> (a -> Int) -- ^ Function used to filter out items during sorting. Quantity, on which the sorting is based.
        -> [a] -- ^ Sorted array.
qsort []     _  = []
qsort (p:ps) f  = (qsort earlier f) ++ [p] ++ (qsort later f)
    where 
        earlier = filter (myFilter (<)  f p) ps
        later   = filter (myFilter (>=) f p) ps

-- | Will filter out all items accoring to provided functions.
--
-- \(O(f + c)\) where both \(f\) and \(c\) are complexities of provided functions.
myFilter :: (Int -> Int -> Bool) -- ^ Function used for ordering items.
                   -> (a -> Int) -- ^ Qualitative function, which is used for sorting.
                            -> a -- ^ First input.
                            -> a -- ^ Second input.
                         -> Bool -- ^ Whether or not was item filtered out.
myFilter f c w1 w2 
    | (c w1) `f` (c w2) = True
    | otherwise         = False

-- | Following method will create groups of words/spaces with equal length, assuming, that the words/spaces are already sorted.
--   Leaving out empty lists where no words/spaces of given length were found.
-- >>> listsSeparate ["hello", "world", "how", "are", "you", "?"] length
-- [["hello","world"],[],["how","are","you"],[],["?"]]
--
-- \(O(n^2 \times f)\), where \(n\) is length of input list and \(f\) is complexity of given function.
listsSeparate :: [a] -> (a -> Int) -> [[a]]
listsSeparate [] _     = []
listsSeparate (s:ss) f = let l = (f s) in listSeparate' (s:ss) f l

-- | Helper function for 'listsSeparate'
--
-- \(O(n^2 \times f)\), where \(n\) is length of input list and \(f\) is complexity of given function.
listSeparate' :: [a] -> (a -> Int) -> Int -> [[a]]
--- listSeparate' [] _ _ = []
listSeparate' _ _ 0  = []
listSeparate' a f l  = let res = (separateLength a f l) in res:(listSeparate' (drop (length res) a) f (l - 1))

-- | Structure for encapsulating 'PuzzleState' together with list of remaining words and spaces. !!
-- 
-- Structure basically represents one node of graph, which is traversed during the process of solving puzzle.
data GraphNode = Node PuzzleState [[String]] [[Space]]
    deriving (Show, Eq)

-- | For encapsulating actual puzzle 2D array and number of remaining empty spaces.
data PuzzleState = Puzzle Int [[Char]]
    deriving (Show, Eq)

-- | Will separate all items of given length.
--
-- >>> separateLength ["ahoj", "svet", "ako", "mas", "sa", "ty", "?", "!"] length 4
-- ["ahoj","svet"]
--
-- \(O(n \times f)\), where \(n\) is length of input list and \(f\) is complexity of given function 
separateLength :: [a] -> (a -> Int) -> Int -> [a]
separateLength [] _ _ = []
separateLength (a:as) f l 
    | (f a) == l = (a:(separateLength as f l))
    | (f a) >  l = separateLength (as) f l
    | otherwise  = [] 

-- step :: [[Char]] -> [[String]] -> [[Space]] -> [[[Char]]]
-- step [] _ _ = []
-- step p [] _ = [p]
-- step p _ [] = [p]
-- step p ((w:ws):wss) ((s:ss):sss) = let (r:rs) = rollArray (w:ws) in (step (insertForSpace p w s) ((ws):wss) ((ss):sss)) ++ (step (insertForSpace p r s) ((rs):wss) ((ss):sss))
-- step p ([]:wss) s = step p wss s

emptyPuzzle :: PuzzleState
emptyPuzzle = Puzzle 5 ["..", ".."]

dummyNode :: GraphNode
dummyNode = Node emptyPuzzle [["aa", "ab", "ab", "bb"], ["b"]] [[Space (0,0) (0,1), Space (0,0) (1,0), Space (0,1) (1,1), Space (1,0) (1,1)], [Space (1,1) (1,1)]]

-- | For given 'GraphNode' function will generate all possible and valid subsequent nodes.
--
-- Behaviour of this function altogether with 'extendLevel' is similiar to DFS algorithm and therefore its complexity is:
--
-- \(O(|V| + |E|)\), where \(V\) is \(n^2 \times m\) where \(n\) is biggest number of words of given length and \(m\) is number of unique lengths and \(E\) is \(V - 1\)
generateLevel :: GraphNode -> Int -> [GraphNode]
generateLevel (Node (Puzzle 0 p) w sp) _ = [(Node (Puzzle 0 p) w sp)]
generateLevel (Node _ [] _) _ = []
generateLevel (Node _ _ []) _ = []
generateLevel (Node p ([]:wss) ([]:sss)) l = let len = length (head wss) in generateLevel (Node p wss sss) len
generateLevel (Node p (_:wss) ([]:sss))  l = let len = length (head wss) in generateLevel (Node p wss sss) len
generateLevel (Node _ ([]:_) _) _ = [] {-- No suitable words for spaces of given length are remaining --}
generateLevel _ 0 = []
generateLevel (Node p ((w:ws):wss) ((s:ss):sss)) l
    | i == 0 = []
    | canBeInsertedPuzzle p s w = extendLevel(newLevelNode:(generateLevel nextInLevelNode (l - 1)))
    | otherwise = extendLevel(generateLevel nextInLevelNode (l - 1))
    where 
        (Puzzle i g)    = p
        nextInLevelNode = Node p ((rollArray $ w:ws):wss) ((s:ss):sss)
        newLevelNode    = Node (insertForSpace p w s) (ws:wss) (ss:sss)

-- | For all given nodes, function will generate new level using 'generateLevel'
--
-- \(O(n^2)\), where \(n\) is the number of nodes in list
extendLevel :: [GraphNode] -> [GraphNode]
extendLevel []     = []
extendLevel (g:gs) = (generateLevel g l) ++ (extendLevel gs)
    where 
        (Node p (w:words) (s:ss)) = g
        l = length w

-- | Determines, wether the given string could be replaced with second string, with '.' symbol being a wildcard.
--
-- >>> canBeReplaced "a.." "abc"
-- True
--
-- >>> canBeReplaced "aa." "abc"
-- False
--
-- \(O(n^2)\) where \(n\) is the length of the second, replacement string.
canBeReplaced :: String -- ^ Template.
              -> String -- ^ Replacement string.
                -> Bool -- ^ Result.
canBeReplaced [] [] = True
canBeReplaced (t:ts) (i:is)
    | lt /= li  = False
    | t == '.'  = canBeReplaced ts is
    | t == i    = canBeReplaced ts is 
    | otherwise = False
    where 
        lt = length (t:ts)
        li = length (i:is)

-- | Will determine, wether given string could be inserted at given 'Space' into given 'PuzzleState'.
--
-- \(O(n + c)\), where \(n\) is the length of the string and \(c\) is complexity of 'peekWithSpace'.
canBeInsertedPuzzle :: PuzzleState -> Space -> String -> Bool
canBeInsertedPuzzle p sp st = canBeReplaced (peekWithSpace p sp) st
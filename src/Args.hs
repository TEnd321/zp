module Args
    ( Options(..)
    , options
    , opts
    ) where

import Options.Applicative

data Options = Opts {
    puzzleInput :: String,
    wordsInput  :: String,
    excludeOnes :: Bool
}

-- | Creating a parser.
options :: Parser Options
options = Opts
    <$> strOption (long "puzzleInput" <> short 'p' <> metavar "PUZZLE_IN" <> value "pIn.txt" <> help "Path to an puzzle input file.")
    <*> strOption (long "wordsInput"  <> short 'w' <> metavar "WORDS_IN" <> value "wIn.txt" <> help "Path to an words input file.")
    <*> flag True False (long "excludeOnes" <> short 'n')

-- | Setting options for main.
opts :: ParserInfo Options
opts = info (options <**> helper)
    (
        fullDesc
     <> progDesc "Solve given crossword puzzle using PUZZLE_IN."
     <> header "A crossword puzzle solver." 
    )
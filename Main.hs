module Main where

import BruteForce            (bruteForce, bruteForce')

import System.Console.GetOpt
import System.Environment    (getArgs)

-- | all possible flags that can be passed to the executable
data Flag = ShowVersion -- ^ displays the version number
          | ShowHelp -- ^ displays a help dialog
          | Count String -- ^ limits the program's output to string of specific length
          | EnableUpperCase -- ^ enables upper case ASCII
          | EnableLowerCase -- ^ enables lower case ASCII
          | EnableNumbers -- ^ enables ASCII numerals (0-9)
          | EnableAdditional String -- ^ enables additional specified characters
          deriving (Eq)

-- | brute's options with description
options :: [OptDescr Flag]
options =
  [ Option ['v'] ["version"] (NoArg ShowVersion) "show version number"
  , Option ['h'] ["help"] (NoArg ShowHelp) "show this dialog"
  , Option ['u'] ["uppercase"] (NoArg EnableUpperCase) "enable upper case"
  , Option ['l'] ["lowercase"] (NoArg EnableLowerCase) "enable lower case"
  , Option ['n'] ["numbers"] (NoArg EnableNumbers) "enable numbers"
  , Option ['a'] ["alphabet"] (ReqArg EnableAdditional "CHARS") "enable specific characters"
  , Option ['c'] ["count"] (ReqArg Count "INT") "only search for strings of specific length"
  ]

-- | parses command line options
bruteOpts
  :: [String] -- ^ an argument vector
  -> IO ([Flag], [String]) -- ^ a tuple of all the parsed flags and additional args
bruteOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, er) -> ioError $ userError (concat er ++ helpDialog)

-- | the help/usage dialog for brute
helpDialog :: String
helpDialog = usageInfo header options
  where header = "Usage: brute [OPTIONS...]"

-- | parses command line arguments and brute-forces according to them
main :: IO ()
main = do
  (opts, _) <- bruteOpts =<< getArgs
  case () of
    ()  | ShowVersion `elem` opts -> putStrLn "brute 0.1.0.0"
        | null opts || ShowHelp `elem` opts -> putStrLn helpDialog
        | otherwise ->
          let alphabet = buildAlphabet opts
              isCount Count{} = True
              isCount _       = False
          in  mapM_ putStrLn $ case filter isCount opts of
            (Count len:_) -> bruteForce' alphabet (read len)
            _ -> bruteForce alphabet
        where
          buildAlphabet :: [Flag] -> String
          buildAlphabet = concatMap $ \opt -> case opt of
            EnableUpperCase     -> ['A'..'Z']
            EnableLowerCase     -> ['a'..'z']
            EnableNumbers       -> ['0'..'9']
            EnableAdditional cs -> cs
            _                   -> []

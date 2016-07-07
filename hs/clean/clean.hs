module Main where

{- 
   Introduction to Clean.hs
-}

import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO
import Control.Monad (forM, when)
import System.FilePath (joinPath, takeExtension)
import qualified System.Directory as Dir

actions :: [(String, FilePath -> IO ())]
actions = [ 
            ("delete", delete),
            ("clean endings", cleanEndings)
          ]

-- types and synonyms
data Flag = Verbose | Endings | Negated | Path String 
            deriving (Show, Eq)

-- utility functions
getPath :: [Flag] -> FilePath
getPath flags = head [ path | Path path <- flags ++ [Path "."]] 

-- recursive directory walk
walk :: FilePath -> IO [FilePath]
walk topdir = do
    names <- Dir.getDirectoryContents topdir
    let proper_names = filter (`notElem` [".", ".."]) names
    paths <- forM proper_names $ \name -> do
        let path = joinPath [topdir, name]
        isDirectory <- Dir.doesDirectoryExist path
        if isDirectory
            then walk path
            else return [path]
    return (concat paths)

-- find files/directories
find :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
find predicate path = do
    names <- walk path
    return (filter predicate names)

-- path functions
display :: FilePath -> IO ()
display path = do
    isfile <- Dir.doesFileExist path
    if isfile then putStrLn $ "|--> " ++ path else do
        isdir <- Dir.doesDirectoryExist path
        if isdir then putStrLn $ "+--> " ++ path else 
            error $ path ++ " does not exist" 

delete :: FilePath -> IO ()
delete path = do
    isfile <- Dir.doesFileExist path
    if isfile then Dir.removeFile path else do
        isdir <- Dir.doesDirectoryExist path
        if isdir then Dir.removeDirectoryRecursive path else
            error $ path ++ " does not exist" 

cleanEndings :: FilePath -> IO ()
cleanEndings path = do
    infile <- openFile path ReadMode
    contents <- hGetContents infile
    hClose infile
    let cleaned = unlines [strip line ++ "\n" | line <- lines contents ]
    outfile <- openFile path WriteMode
    hPutStr outfile cleaned
    hClose outfile
    where strip = unwords . words

-- matcher functions
endswith :: [String] -> FilePath -> Bool
endswith patterns path = or [takeExtension path == p | p <- patterns]

-- primary processing function
process :: [Flag] -> [String] -> IO ()
process flags patterns = do
    let path = getPath flags
    when (elem Verbose flags) $ dump flags patterns
    if elem Negated flags then do
        files <- find (not . endswith patterns) path 
        manage files flags
        else do
            files <- find (endswith patterns) path
            manage files flags
    where
        manage files flags
            | elem Endings flags = execute "clean endings" files
            | otherwise = execute "delete" files
    
        execute cmd files = do
            mapM_ display files
            let (Just action) = lookup cmd actions
            putStrLn $ "Apply '" ++ cmd ++ "' to all (y/n)?"
            answer <- getChar
            if answer == 'y' then
                mapM_ action files 
                else error $ cmd ++ " action aborted"

        dump flags patterns = do
                putStrLn $ "options: " ++ show flags
                putStrLn $ "patterns: " ++ show patterns

-- commandline options
options :: [OptDescr Flag]
options = [
    Option "p" ["path"]    (ReqArg Path "PATH") "set path to process",
    Option "v" ["verbose"] (NoArg Verbose)      "show verbose output",
    Option "e" ["endings"] (NoArg Endings)      "clean line endings",
    Option "n" ["negated"] (NoArg Negated)      "clean all except patterns"
    ]

-- main entrypoint
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        ([], [], []) -> error $ usageInfo header options 
        (flags, [],  []) -> error $ usageInfo header options
        (flags, nonOpts, []) -> process flags nonOpts
        (_, _,     msgs) -> error $ concat msgs ++ usageInfo header options
    where header = "Usage: clean [options] patterns"


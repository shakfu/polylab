module Main where

import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Control.Monad (forM_, when)
import System.FilePath (joinPath)
import System.Cmd (system)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.Posix.Directory (changeWorkingDirectory)

defaultDir :: FilePath
defaultDir = "/home/sa/src"

actions :: [(String, [String])]
actions = [
--      vcs       actions
        (".bzr", ["bzr pull", "bzr update"]),
        (".hg",  ["hg pull", "hg update"]),
        (".svn", ["svn update"]),
        (".git", ["git pull"])
    ]

-- run shell (system) command
run :: String -> IO ()
run action = do
    exitcode <- system action
    if show exitcode == "ExitSuccess" then
        putStrLn "\n"
        else putStrLn $ action ++ " Error"

-- update directory
update :: FilePath -> IO ()
update [] = error "nothing to process"
update dir = do
    contents <- getDirectoryContents dir
    let vcsfound = filter (`elem` vcstypes) contents
    if not (null vcsfound) then do
        changeWorkingDirectory dir
        putStrLn $ "\nupdating ... " ++ dir
        mapM_ run $ fromJust $ lookup (head vcsfound) actions
        else putStrLn $ "error: no vcs directory found in " ++ dir
    where
        vcstypes = [fst action | action <- actions]

-- primary command line options processing
process :: FilePath -> IO ()
process topdir = do
    contents <- getDirectoryContents topdir
    let proper_names = filter (`notElem` [".", "..", "-"]) contents
    forM_ proper_names $ \name -> do
        let path = joinPath [topdir, name]
        isDirectory <- doesDirectoryExist path
        when isDirectory $ update path

-- main entrypoint
main = do
    args <- getArgs
    putStrLn $ "args: " ++ show args
    if length args > 0 then
        forM_ args $ \arg -> process arg
        else process defaultDir

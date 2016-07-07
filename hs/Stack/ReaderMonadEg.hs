module Main where

import Control.Monad.Reader

data Config = Config { 
    logFile :: FilePath
  } deriving Show

type Program = ReaderT Config IO

readLog :: Program String
readLog = do 
    Config logFile <- ask
    liftIO $ readFile logFile

writeLog :: String -> Program ()
writeLog message = do 
    Config logFile <- ask
    liftIO $ writeFile logFile message
    --x <- liftIO $ readFile logFile
    --liftIO $ writeFile logFile $ x ++ message

getUserInput :: Program String
getUserInput = do
    liftIO $ putStrLn "Input a log" 
    input <- liftIO $ getLine
    writeLog $ "Input: " ++ input
    return input

runProgram :: Program ()
runProgram = do 
    input <- getUserInput
    liftIO $ putStrLn $ ("You wrote: " ++ input ++ "\n")

runApp :: Config -> IO ()
runApp = runReaderT runProgram

runDefault :: IO ()
runDefault = runApp $ Config {logFile = "my.log"}

--main = do 
--    let cfg = Config {logFile = "my.log"}
--    runReaderT runProgram cfg

-- e.g runApp (constrainedCount 0 "..") 1
--runApp :: App a -> Int -> IO (a, AppState)
--runApp k maxDepth =
--    let config = AppConfig maxDepth
--        state = AppState 0
--    in runStateT (runReaderT k config) state


module Main where

import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig { 
    logFile :: FilePath
  } deriving Show

data AppState = AppState { 
    status :: String
  } deriving Show


type Program = ReaderT AppConfig IO
--type App = ReaderT AppConfig (StateT AppState IO)


readLog :: Program String
readLog = do 
    AppConfig logFile <- ask
    liftIO $ readFile logFile

writeLog :: String -> Program ()
writeLog message = do 
    AppConfig logFile <- ask
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

runApp :: AppConfig -> IO ()
runApp = runReaderT runProgram

runDefault :: IO ()
runDefault = runApp $ AppConfig {logFile = "my.log"}

--main = do 
--    let cfg = AppConfig {logFile = "my.log"}
--    runReaderT runProgram cfg

-- e.g runApp (constrainedCount 0 "..") 1
--runApp :: App a -> Int -> IO (a, AppState)
--runApp k maxDepth =
--    let config = AppConfig maxDepth
--        state = AppState 0
--    in runStateT (runReaderT k config) state


{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ErrorStack where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
-- import Control.Monad.Except


data AppConfig = AppConfig {
        logFile :: FilePath
    } deriving Show

data AppState = AppState {
        status :: String
    } deriving Show

type AppLog = [String]


newtype Program a = Program {
        runProg :: ReaderT AppConfig (StateT AppState (WriterT AppLog IO)) a
    } deriving ( Monad
               , MonadIO
               , MonadReader AppConfig
               , MonadState  AppState 
               , MonadWriter AppLog 
               )

instance Functor Program where
    fmap = liftM

instance Applicative Program where
    pure = return
    (<*>) = ap

program :: Program ()
program = do
    -- get configuration
    AppConfig logFile <- ask
    -- get state
    AppState status <- get
    -- write to log
    tell ["WARN: msg 1"]

    liftIO . putStrLn $ ("logFile: " ++ logFile)

    return ()


runProgram :: AppConfig -> Program a -> IO ((a, AppState), AppLog)
runProgram c p = runWriterT (runStateT (runReaderT (runProg p) c) state)
    where 
        state = AppState { status = "start"} 

test :: IO (((), AppState), AppLog)
test = runProgram config program
    where
        config = AppConfig { logFile = "my.log"}


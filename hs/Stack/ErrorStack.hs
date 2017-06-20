{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ErrorStack where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except


data AppConfig = AppConfig {
        logFile :: FilePath
    } deriving Show

data AppState = AppState {
        status :: String
    } deriving Show

type AppLog = [String]

type AppError = String

newtype Program a = Program {
        runProg :: ReaderT AppConfig (StateT AppState (WriterT AppLog (ExceptT AppError IO))) a
    } deriving ( Monad
               , MonadIO
               , MonadReader AppConfig
               , MonadState  AppState 
               , MonadWriter AppLog 
               , MonadError  AppError
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

    when (status /= "start") $
        throwError "the app has started"

runProgram :: AppConfig -> Program a -> IO (Either AppError ((a, AppState), AppLog))
runProgram c p = runExceptT (runWriterT (runStateT (runReaderT (runProg p) c) state))
    where 
        state = AppState { status = "start"} 

test :: IO (Either AppError (((), AppState), AppLog))
test = runProgram config program
    where
        config = AppConfig { logFile = "my.log"}



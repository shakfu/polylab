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


-- type App = ReaderT AppConfig (StateT AppState (WriterT AppLog ((ExceptT String IO))))

-- type App a = StateT String (WriterT [String] a)


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

runR :: Program a -> AppConfig -> StateT AppState (WriterT AppLog IO) a
runR p = runReaderT (runProg p)

runS :: Program a -> AppConfig -> AppState -> WriterT AppLog IO (a, AppState)
runS p c = runStateT  (runR p c)

runW :: Program a -> AppConfig -> AppState -> IO ((a, AppState), AppLog)
runW p c s = runWriterT (runS p c s)  

runProgram1 :: Program a -> AppConfig -> AppState -> IO ((a, AppState), AppLog)
runProgram1 p c s = runWriterT (runStateT (runReaderT (runProg p) c) s)

runProgram2 :: Program a -> AppConfig -> IO ((a, AppState), AppLog)
runProgram2 p c = runWriterT (runStateT (runReaderT (runProg p) c) state)
    where 
        state = AppState { status = "start"} 

runProgram :: AppConfig -> Program a -> IO ((a, AppState), AppLog)
runProgram c p = runWriterT (runStateT (runReaderT (runProg p) c) state)
    where 
        state = AppState { status = "start"} 

test :: IO (((), AppState), AppLog)
test = runProgram config program
    where
        config = AppConfig { logFile = "my.log"}


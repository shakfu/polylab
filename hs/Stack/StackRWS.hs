module Main where

import Debug.Trace
import Control.Monad.RWS.Lazy


data AppConfig = AppConfig {
      defaultColor :: String
    } deriving Show


data AppState = AppState {
      status :: String
    } deriving Show


type App a = RWS AppConfig [String] AppState a


program :: App ()
program = do
    config <- ask
    tell [defaultColor config]
    s0     <- get
    let s1 = AppState { status = "s1" }
    put s1
    tell ["msg: " ++ status s1]
    -- return ()


runApp :: AppConfig -> ((), AppState, [String])
runApp = flip (runRWS program) initialState
    where
        initialState = AppState{ status = "" }



--program :: App ()
--program = do
--    config <- ask
--    s0     <- get
--    let s1 = AppState { status = "s1: " ++ (status s0) ++ "." }
--    put s1
--    tell ["msg: " ++ (status s1)]


--runApp :: AppConfig -> ((), AppState, [String])
--runApp = flip (runRWS program) initialState
--    where
--        initialState = AppState{status=""}

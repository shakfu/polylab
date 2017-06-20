{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.RWS.Lazy

data AppConfig = AppConfig {
      defaultColor :: String
    } deriving Show

data AppState = AppState {
      status :: String
    } deriving Show

-- type RWS r w s = RWST r w s Identity

-- A monad containing an environment of type r, 
-- output of type w and 
-- an updatable state of type s.


type Contrived a = RWS Int [Int] Int a

contrived :: Contrived ()
contrived = do
    env <- ask         -- read from the fixed environment
    s0  <- get         -- grab the current state
    let s1 = s0 + env  -- increment the current state by the environment value
    put s1             -- update the state to the new, incremented value
    tell [s1]          -- log the new value


runContrived :: Int -> Int -> ((), Int, [Int])
runContrived = runRWS contrived


type App a = RWS AppConfig [String] AppState a

program :: App ()
program = do
    config <- ask
    s0     <- get
    let s1 = AppState { status = "s1: " ++ (status s0) ++ "." }
    put s1
    tell ["msg: " ++ (status s1)]

--runApp :: String -> String -> ((), String, [String])
--runApp = runRWS program

runApp :: AppConfig -> AppState -> ((), AppState, [String])
runApp = runRWS program


--type App a = RWS String [String] String a

--program :: App ()
--program = do
--    env <- ask
--    s0  <- get
--    let s1 = "s1: " ++ s0 ++ "." ++ env
--    put s1
--    tell ["msg: " ++ s1]

--runApp :: String -> String -> ((), String, [String])
--runApp = runRWS program
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

-- import Control.Monad.Trans.RWS.Lazy

type App a = StateT String (WriterT [String] a)

data AppConfig = AppConfig {
        defaultColor :: String
    } deriving Show

data AppState = AppState {
        status :: String
    } deriving Show

type AppLog = [String]

newtype Program a = Program {
        runProgram :: ReaderT AppConfig (StateT AppState (WriterT AppLog IO)) a
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


test :: String -> Program ()
test s = do
    -- get configuration
    AppConfig defaultColor <- ask
    -- get state
    AppState status <- get
    -- write to log
    tell ["WARN: msg 1"]

    liftIO . putStrLn $ ("color: " ++ defaultColor)

    return ()
    

data Person = Person 
    { personName :: String
    , personAge  :: Int
    }

data Car = Car 
    { carName  :: String
    , carType  :: String
    }

class Model a where
    display :: a -> String


instance Model Person where
    display (Person name age) = ("Person: " ++ name ++ " " ++ show age)



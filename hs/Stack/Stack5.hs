{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stack5 where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad
import Control.Applicative
import Control.Monad.Except

data DbError = DbConnectionError
             | DbResultError
newtype Db a = Db
    { unDb :: ExceptT DbError (ReaderT DbEnv IO) a }
runDb :: DbEnv -> Db a -> IO (Either DbError a)

data LogicError = LogicError1
                | LogicError2
newtype Logic a = Logic
    { unLogic :: ExceptT LogicError (ReaderT LogicEnv IO) a }
runLogic :: LogicEnv -> Logic a -> Either LogicError a

data AppEnv = AppEnv { appEnvDb :: DbEnv, appEnvLogic :: LogicEnv }
data AppError = AppDbError DbError
              | AppLogicError LogicError

newtype App a = App
    { unApp :: ExceptT AppError (ReaderT AppEnv IO) a }
runApp :: AppEnv -> App a -> IO (Either AppError a)


liftLogic :: Logic a -> App a
liftLogic logic = do
    env <- asks appEnvLogic
    either (throwError . AppLogicError) pure $ runLogic env logic

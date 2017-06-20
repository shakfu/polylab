{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Stack6 where

-- see: http://carlo-hamalainen.net/blog/2015/7/20/classy-mtl

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Text

-- DB
type DbConnection = Text
type DbSchema     = Text

data DbConfig = DbConfig
    { _dbConn :: DbConnection
    , _schema :: DbSchema
    }

data DbError = QueryError Text | InvalidConnection

-- Networking
type Port = Integer
type Ssl  = Text

data NetworkConfig = NetworkConfig
    { _port  :: Port
    , _ssl   :: Ssl
    }

data NeworkError = Timout Int | InactiveServer

-- App
data AppConfig = AppConfig
    { _appDbConfig   :: DbConfig 
    , _appNetConfig  :: NetworkConfig
    }

data AppError = AppDbError  { dbError  :: DbError      }
              | AppNetError { netError :: NetworkError }


makeClassy ''DbConfig
makeClassy ''NetworkConfig
makeClassy ''AppConfig

makeClassyPrisms ''DbError
makeClassyPrisms ''NetworkError
makeClassyPrisms ''AppError


loadFromDb :: ( MonadError e m,
                MonadReader r m,
                AsDbError e,
                HasDbConfig r,
                MonadIO m) =m Text
loadFromDb = do
    -- Due to "MonadReader r m" and "HasDbConfig r"
    -- we can ask for the database config:
    rdr <- ask
    let dbconf  = rdr ^. dbConfig :: DbConfig

    -- We can ask for the connection string directly:
    let connstr  = rdr ^. dbConn :: DbConnection

    -- We have "AsDbError e", so we can throw a DB error:
    throwError $ (_InvalidConnection #) ()
    throwError $ (_QueryError #) "Bad SQL!"

    return "foo"


sendOverNet :: ( MonadError e m,
                 MonadReader r m,
                 AsNetworkError e,
                 AsAppError e,
                 HasNetworkConfig r,
                 MonadIO m) => Text -> m ()
sendOverNet mydata = do
    -- We have "MonadReader r m" and "HasNetworkConfig r"
    -- so we can ask about the network config:
    rdr <- ask
    let netconf = rdr ^. networkConfig  :: NetworkConfig
        p       = rdr ^. port           :: Port
        s       = rdr ^. ssl            :: Ssl

    liftIO $ putStrLn $ "Pretending to connect to the network..."

    -- We have "AsNetworkError e" so we can throw a network error:
    throwError $ (_NetworkError #) (Timeout 100)

    -- We have "AsAppError e" so we can throw an application-level error:
    throwError $ (_AppNetError #) (Timeout 100)

    return ()

loadAndSend :: ( AsAppError e,
                 AsNetworkError e,
                 AsDbError e,
                 HasNetworkConfig r,
                 HasDbConfig r,
                 MonadReader r m,
                 MonadError e m,
                 MonadIO m) => m ()
loadAndSend = do
    liftIO $ putStrLn "Loading from the database..."
    t <- loadFromDb
    liftIO $ putStrLn "Sending to the network..."
    sendOverNet t



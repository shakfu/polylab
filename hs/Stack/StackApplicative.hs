{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Monad.Reader

data MyData = MyData Int Int deriving Show

newtype MyMonad a = MyMonad (ReaderT [Int] Maybe a)
    deriving (Functor, Applicative, Monad, MonadReader [Int])

runMyMonad :: MyMonad a -> [Int] -> Maybe a
runMyMonad (MyMonad m) = runReaderT m

myError :: MyMonad a
myError = MyMonad $ lift Nothing

get2Sum :: MyMonad Int
get2Sum = do
    myData <- ask
    let fst2 = take 2 myData
    case length fst2 of
        2 -> return $ sum fst2
        _ -> myError

myDataFromApplicative = MyData <$> get2Sum <*> get2Sum

main = do
    print $ runMyMonad myDataFromApplicative [1,2]
    print $ runMyMonad myDataFromApplicative [1]

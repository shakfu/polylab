{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad.Writer (writer, runWriter, Writer)
import Control.Monad.Reader
import Control.Monad.State


data Asset = Asset 
    { assetId  :: String
    , assetTag :: String
    } deriving Show

type Stack = [Int]

data Machine state event signal = Machine
    { mCurState :: state
    , mTransFunction :: state -> event -> (state, signal)
    }

stepMachine :: Machine state event signal -> event -> (Machine state event signal, signal)
stepMachine machine event = (machine {mCurState = newState}, output)
    where
        curState = mCurState machine
        (newState, output) = mTransFunction machine curState event

createMachine :: state -> (state -> event -> (state, signal)) -> Machine state event signal
createMachine = Machine        

logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x, ["Got number: " ++ show x])  

multiLog :: Writer [String] Int  
multiLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)

displayAsset :: Asset -> IO ()
displayAsset asset = putStrLn (assetId asset)

asset1 :: Asset
asset1 = Asset { 
    assetId  = "sa", 
    assetTag = "sa"
}

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x, xs)  
  
push :: Int -> State Stack ()  
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int  
stackManip = do  
    push 3
    a <- pop  
    pop

stackManip' :: State Stack Int  
stackManip' = do  
    push 3
    pop  
    pop

stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8 

moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()

stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]



main :: IO ()
main = do
    displayAsset asset1
    putStrLn . head . snd . runWriter $ multiLog 

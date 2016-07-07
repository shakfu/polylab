module Hello where

import Control.Monad.State
import Control.Monad.Identity



test1 = do
    a <- get
    modify (+1)
    b <- get
    return (a,b)

test2 = do
    a <- get
    modify (++"1")
    b <- get
    return (a,b)

go1 = evalState test1 0
go2 = evalState test2 "0" 


test3 = do
    modify (+ 1)
    lift $ modify (++ "1")
    a <- get
    b <- lift get
    return (a,b)

go3 = runIdentity $ evalStateT (evalStateT test3 0) "0"


test5 = do
    modify (+ 1)
    a <- get
    lift (print a)
    modify (+ 1)
    b <- get
    lift (print b)

go5 = evalStateT test5 0

test7 = do
    modify (+ 1)
    lift $ modify (++ "1")
    a <- get
    b <- lift get
    return (a,b)

go7 = evalState (evalStateT test7 0) "0"


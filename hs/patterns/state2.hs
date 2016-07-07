-- a concrete and simple example of using the State monad
import Control.Monad.State
 
-- non monadic version of a very simple state example
-- the State is an integer.
-- the value will always be the negative of of the state
 
type MyState = Int
 
valFromState :: MyState -> Int
valFromState s = -s
nextState :: MyState -> MyState
nextState x = 1+x
 
type MyStateMonad  = State MyState 
 
-- this is it, the State transformation.  
-- Add 1 to the state, return -1*the state as the computed value.
getNext ::   MyStateMonad  Int
getNext  = state  (\st -> let st' = nextState(st) in (valFromState(st'), st') )
 
 
 
-- advance the state three times.  
inc3 :: MyStateMonad Int
inc3 =  getNext >>= \x ->
        getNext >>= \y ->
        getNext >>= \z ->
        return z                        
 
-- advance the state three times with do sugar
inc3Sugared :: MyStateMonad Int
inc3Sugared = do x <- getNext
                 y <- getNext
                 z <- getNext
                 return z
 
-- advance the state three times without inspecting computed values
inc3DiscardedValues :: MyStateMonad Int
inc3DiscardedValues =    getNext >> getNext >> getNext
 
-- advance the state three times without inspecting 
-- computed values with do sugar
inc3DiscardedValuesSugared::MyStateMonad Int
inc3DiscardedValuesSugared =    do 
                            getNext
                            getNext
                            getNext
 
 
-- advance state 3 times, compute the square of the state
inc3AlternateResult :: MyStateMonad Int
inc3AlternateResult = do  getNext
                          getNext
                          getNext
                          s<-get
                          return (s*s)
 
 
-- advance state 3 times, ignoring computed value, and then once more
inc4 :: MyStateMonad Int
inc4 = do 
          inc3AlternateResult
          getNext
 
main = 
     do
          print (evalState inc3 0)                         -- -3
          print (evalState inc3Sugared 0)                  -- -3
          print (evalState inc3DiscardedValues 0)          -- -3
          print (evalState inc3DiscardedValuesSugared 0)   -- -3
          print (evalState inc3AlternateResult 0)          -- 9
          print (evalState inc4 0)                         -- -4

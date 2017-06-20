-- Example by Mark Carroll from http://www.haskell.org/hawiki/MonadState

module NestedMonad where

import Control.Monad.State
import Data.List

-- I/O monad function returns length of named file

get_length :: String -> IO Int

get_length name = do 
    contents <- readFile name
    return (length (lines contents))

-- Function returns state consisting of (String, Int)
-- combined with an IO Monad:

update_best_with :: (String, Int) -> StateT (String, Int) IO ()

update_best_with (current_name, current_count) = do 
    (best_name, best_count) <- get
    if current_count > best_count
        then put (current_name, current_count)
        else return ()

-- This function likewise

do_next :: String -> StateT (String, Int) IO ()

do_next name = do 
    count <- lift $ get_length name
    update_best_with (name, count)

-- This function returns an IO Monad, and returns the final state
-- from the state Monad.
--
-- Note the initial state provided as final argument to execStateT
-- sequence_ takes a list of monads of some given type and constructs
-- a new monad that sequences the elements of the list.

longest_file :: [String] -> IO (String, Int)

longest_file names =
    execStateT (sequence_ (map do_next names)) ("none", 0)

-- And here's the main program that drives it all.

main = do 
    (name, count) <- longest_file ["./derive.hs", "./transform.hs", "./lease.hs"]
    putStrLn ("Longest file is " ++ name ++ " which is " ++ show count ++ " lines long")

module Mylib where

import CString

adder :: Int -> Int -> IO Int
adder x y = return (x+y)

foreign export stdcall adder :: Int -> Int -> IO Int



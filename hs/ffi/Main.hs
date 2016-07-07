module Main where

import Foreign.C.Types(CDouble(..), CInt(..))
import Foreign.C.String(CString(..), peekCString)
import Model.Person

foreign import ccall "math.h sin"
    c_sin :: CDouble -> CDouble

foreign import ccall "cfuncs.h add"
    c_add :: CInt -> CInt -> CInt

foreign import ccall "cfuncs.h message"
    c_msg :: CString

main = do
    let p = Person {name = "Sam", age = 10}
    putStrLn $ "c_sin 90    = " ++ show (c_sin 90.0)
    putStrLn $ "c_add 10 20 = " ++ show (c_add 10 20)
    msg <- peekCString c_msg
    putStrLn $ "c_msg       = " ++ msg
    putStrLn $ "Hello " ++ (name p)


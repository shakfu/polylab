{-# LANGUAGE ForeignFunctionInterface #-}
module Test where
 
import Foreign.C.Types

foreign import ccall "math.h sin"
    c_sin :: CDouble -> CDouble

foreign export ccall
    hsfun :: CInt -> IO CInt


hsfun :: CInt -> IO CInt
hsfun x = do
    putStrLn "Hello World"
    return (42 + x)
 
fastsin :: Double -> Double
fastsin x = realToFrac (c_sin (realToFrac x))



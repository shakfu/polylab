{-# LANGUAGE ForeignFunctionInterface #-}
{-| 
    This is the description for module "Financial"
-}

module Simple where

{-
import Data.List
import Data.Maybe
import Foreign.C.Types
import Foreign.C.String
-}

-- can be CFloat or Float

pmt :: Float -> Float -> IO Float
pmt x y = return (x + y)

foreign export ccall
    pmt :: Float -> Float -> IO Float



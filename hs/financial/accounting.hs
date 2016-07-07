{-| 
    This is the description for module "Financial"
-}

module Accounting where

import Data.List
import Data.Maybe

-- * Data
cashflows = [-100.0, 60.0, 60.0, 60.0]


-- * Financial Functions

{-|
    monthly lease payment calculator (accurate)
-}
pmt :: Double -> Double -> Double -> Double -> Double
pmt capital residual interest periods = a / b
    where 
        a = capital - (residual / 1 + interest) ** periods
        b = ((1 - 1/(1 + interest)**periods) / interest)

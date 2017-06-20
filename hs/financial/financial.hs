{-# LANGUAGE ForeignFunctionInterface #-}
{-| 
    This is the description for module "Financial"
-}

module Financial where

import Data.List
import Data.Maybe
import Foreign.C.Types
import Foreign.C.String

-- * Data
cashflows = [-100.0, 60.0, 60.0, 60.0]

-- * Numerical Methods

bisection tol reltol f a b | tol < 0 || reltol < 0 =
                               error "bisection needs positive tolerances"
                           | abs fa <= tol = Just a -- a is a root
                           | abs fb <= tol = Just b -- b is a root
                           | fa*fb > 0 = Nothing -- IVT not guaranteed
                           | a > b = bis b a fb fa -- exchange endpoints
                           | otherwise = bis a b fa fb -- standard case
    where
      fa = f a                  -- function evaluated only once at each
      fb = f b                  -- endpoint, store them here
      bis a b fa fb = let m = (a+b)/2
                          fm = f m in
                      if (b-a) <= reltol*(1+abs(a)+abs(b)) || abs fm <= tol
                      then Just m
                      -- implicit assumption: fa * fb > 0
                      else if fm*fa < 0 -- new interval: [a,m]
                           then bis a m fa fm
                           else bis m b fm fb -- new interval: [m,b]

-- uniroot is a specialisation of bisection, with commonly used
-- tolerance values
uniroot = bisection 1e-20 1e-15

mysqrt x = uniroot f 0 x
       where f y = y**2-x



-- * Financial Functions

{-|
    monthly lease payment calculator (accurate)
-}
pmt :: Double -> Double -> Double -> Double -> Double
pmt capital residual interest periods = a / b
    where 
        a = capital - (residual / 1 + interest) ** periods
        b = ((1 - 1/(1 + interest)**periods) / interest)

foreign export ccall
    pmt :: Double -> Double -> Double -> Double -> Double


{-|
    monthly lease payment calculator (approximate)
-}
lease :: Double -> Double -> Double -> Double -> Double
lease capital residual interest periods = depreciation + finance + tax
    where 
        depreciation = (capital - residual) / periods
        finance = (capital + residual) * money_factor
        money_factor = interest / 2400.0
        tax = 0.0


{-|
    payback period (years) cashflows include investment
-}
payback :: [Double] -> Double
payback (investment : cashflows) = payback1 (negate investment) cashflows

{-|
    payback period (years) with investment
-}
payback1 :: Double -> [Double] -> Double
payback1 investment cashflows
    | null cashflows = error "no cashflows"
    | sum(cashflows) < investment = error "insufficient cashflows"
    | otherwise = a + (b / c)
    where
        a = fromIntegral years
        b = investment - cumulative !! (years - 1)
        c = cumulative !! years - cumulative !! (years - 1)
        years = length $ takeWhile (< investment) cumulative
        cumulative = scanl1 (+) cashflows



{-|
    net present value
-}
npv :: [Double] -> Double -> Double
npv cashflows discount_rate = sum [discounted cf | cf <- cashflows]
    where 
        discounted cf = cf / (1 + discount_rate) ** index cf
        index i = fromIntegral $ fromJust (elemIndex i cashflows)

{-|
    internal rate of return

The IRR is the annualized effective compounded return rate which can be earned on the invested capital, i.e., the yield on the investment. Put another way, the internal rate of return for an investment is the discount rate that makes the net present value of the investment's income stream total to zero.

-}
irr :: [Double] -> Double
irr cashflows = foldl (update) 1 [1..100]
    where 
        update rate i = rate * (1 - (npv cashflows rate) / investment)
        investment = head cashflows

{-|
    weighted average cost of capital

A calculation of a firm's cost of capital in which each category of capital is proportionately weighted. All capital sources - common stock, preferred stock, bonds and any other long-term debt - are included in a WACC calculation. All else help equal, the WACC of a firm increases as the beta and rate of return on equity increases, as an increase in WACC notes a decrease in valuation and a higher risk.
    
    >    WACC = wd (1-T) rd + we re
    >
    >           wd = debt portion of value of corporation
    >            T = tax rate
    >           rd = cost of debt (rate)
    >           we = equity portion of value of corporation
    >           re = cost of internal equity (rate)
    
-}
wacc :: Double -> Double -> Double -> Double -> Double -> Double
wacc equity_cost debt_cost debt equity capital =
    (equity / capital) * equity_cost + 
    (debt / capital) * debt_cost * (1 - tax_rate)
    where
        capital = debt + equity 
        tax_rate = 0

{-|
    present value of an annuity

A cash flow stream with a limited number (n) of periodic payments (C), receivable at times 1 through n, is an annuity. Future payments are discounted by the periodic rate of interest (i). The present value of this ordinary annuity is determined with this formula
-}
annuity :: Double -> Double -> Double -> Double
annuity c n i = c/i * (1 - (1/(1+i)**n))

{-|
    future value
    
where PV is the present value or principal, t is the time in years, and r stands for the per annum interest rate. Simple interest is rarely used, as compounding is considered more meaningful.
-}
fv :: Double -> Double -> Double -> Double
fv pv i n = pv * (1 + i)**n


{-|
    discounted cash flow (or discounted present value)
    dpv fv i n = fv / (1 + i)**n 
-}
dpv :: [Double] -> Double -> Double -> Double
dpv fvs i n = sum [fv / (1+i)**n | fv <- fvs ]

mirr reinvest_rate finance_rate cashflows = (a / b)**c - 1.0
    where
        n = fromIntegral $ length cashflows
        positives = map (\c -> if c > 0 then c else 0.0) cashflows
        negatives = map (\c -> if c < 0 then c else 0.0) cashflows
        a = (negate $ npv positives reinvest_rate) * (1.0 + reinvest_rate)**n 
        b = (npv negatives finance_rate) * (1.0 + finance_rate)   
        c = (1.0 / (n - 1.0))

summary :: [Double] -> IO ()
summary cashflows = do
    putStrLn $ "payback period: " ++ (show $ payback cashflows)
    putStrLn $ "npv: " ++ (show $ npv cashflows 0.05)
    putStrLn $ "irr: " ++ (show $ irr cashflows)












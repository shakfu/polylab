module Sap.Stat where

import Data.List
import Data.Function

{- | mean of list of values

>>> mean [1..100]
50.5

-}
mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / genericLength xs

{- | Median

>>> median [1..100]
50.0

-}
median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

--median' :: [a] -> a
median' :: (Fractional a, Ord a) => [a] -> a
median' xs'
    | null xs  = 0
    | odd len = xs !! mid
    | even len = (xs !! (mid - 1) + xs !! mid) / 2
  where 
    xs = sort xs'
    len = length xs
    mid = len `div` 2

{- | Mode returns the mode of the list

>>> mode [1.0, 1.0, 2.0, 2.0, 3.0]
1.0

-}
mode :: Ord a => [a] -> a
mode xs = head . last $ sortBy (compare `on` length) (group xs)

{- | Sample variance is a measure of how far a set of numbers is spread out

>>> variance [1.0, 1.0, 2.0, 2.0, 3.0]
0.56

-}
variance :: (RealFrac a, Fractional b) => [a] -> b
variance = mean . distancesFromMean where 
    distancesFromMean xs = map ((^(2::Integer)) . subtract (mean xs)) xs

{- | Standard deviation of sample

>>> stddev [1.0, 1.0, 2.0, 2.0, 3.0]
0.7483314773547883

-}
stddev :: RealFloat a => [a] -> a
stddev = sqrt . variance

stdscore :: RealFloat a => a -> [a] -> a
stdscore x xs = (x - mean xs) / stddev xs

factorial :: Int -> Int
factorial n = product [1..n]

-- | The number of ways to choose k elements from an n-element set.
binorm :: Int -> Int -> Int
binorm n k = product [k+1..n] `div` product [1..n-k]

{- | The probability for k events of chance p to occur at the same 
     time in an n-element set.
-}
binormProb :: Floating a => Int -> Int -> a -> a
binormProb n k p = fromIntegral (binorm n k) * (p ** fromIntegral k) * ((1.0 - p) ** fromIntegral (n-k))

-- | Probability density function
probDensity :: Floating a => a -> a
probDensity x = factor * exp (-0.5 * x * x)
                where factor = 1.0 / sqrt (2.0 * pi)

-- | Gaussian function
gaussian :: Floating a => a -> a -> a -> a
gaussian x mu sigma = 1.0 / sigma * probDensity ((x - mu) / sigma)

{- | Measures how much two random variables change together.
     covariance xs xs = variance xs
-}
{-# ANN covariance "HLint: ignore" #-}
covariance :: RealFrac a => [a] -> [a] -> a
covariance xs ys = mean $ map distance (zip xs ys)
                   where xmu = mean xs
                         ymu = mean ys
                         distance (x, y) = (x-xmu)*(y-ymu)

-- Pearson's product-moment coefficient r
correlation :: RealFloat a => [a] -> [a] -> a                       
correlation xs ys = covariance xs ys / (stddev xs * stddev ys)
          
{- Gets the slope b of the linear regression line through the data.
   (sum $ distancesFromMean2 xs ys) / (sum $ distancesFromMean ys)
-}
slope :: RealFrac a => [a] -> [a] -> a
slope xs ys = covariance xs ys / variance xs

-- Gets the intercept a of the linear regression line through the data.
intercept :: RealFrac a => [a] -> [a] -> a
intercept xs ys = mean ys - (b * mean xs) 
                  where b = slope xs ys

-- Gets the size of the confidence interval using -- the t-Distribution value [tfactor].
confinv :: RealFloat a => [a] -> a -> a
confinv xs tfactor = tfactor * (stddev xs / sqrt (genericLength xs))

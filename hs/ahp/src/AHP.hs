{-# LANGUAGE FlexibleContexts #-}

module AHP where

{-

Simple implementation of Analytic hierarchy process

see: https://en.wikipedia.org/wiki/Analytic_hierarchy_process

-}

import Data.List (lookup)
import Data.Maybe (fromJust)
-- import Data.Packed.Matrix
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel
import Numeric.LinearAlgebra
-- import Numeric.Container
-- import Numeric.LinearAlgebra.Algorithms (eig, Field(..))


tableCR :: (Fractional t1, Num t) => [(t, t1)]
tableCR = [
        (1,  0),
        (2,  0),
        (3,  0.58),
        (4,  0.9),
        (5,  1.12),
        (6,  1.24),
        (7,  1.32),
        (8,  1.41),
        (9,  1.45),
        (10, 1.49)
      ]

-- items to compare
-- where n = no of items to compare
nComparisons :: Fractional a => a -> a
nComparisons n = n * (n - 1) / 2

m0 :: Matrix Double
m0 = fromLists
    [ [1  , 2  , 3]
    , [1/2, 1  , 2]
    , [1/3, 1/2, 1]
    ]

getSize :: Num b => Matrix t -> b
getSize m = if rows m == cols m then fromIntegral $ rows m
               else error "matrix not square"

analyzeEigens :: Field t => Matrix t -> (Complex Double, Vector (Complex Double))
analyzeEigens m = (principalEigenValue, principalEigenVector)
    where
        (eigenvalues, eigenvectors) = eig m
        principalEigenValue = maxElement eigenvalues
        principalEigenVector = takeDiag eigenvectors


-- mapVector :: (Storable a, Storable b) => (a -> b) -> Vector a -> Vector b

normPrincipalEigenVector :: (RealElement b, Convert b, Container Vector b) =>
     Vector (Complex b) -> Vector b
normPrincipalEigenVector pevec = cmap (/ total) vec
    where
        (vec, errs) = fromComplex pevec
        total = sumElements vec

consistencyIndex :: RealFloat t => Complex t -> Complex t -> t
consistencyIndex peval n = toDouble $ (peval - n) / (n - 1)
    where
        toDouble (d :+ c) = d

consistencyRatio :: (Eq a1, Fractional a, Num a1) => a -> a1 -> a
consistencyRatio ci n = ci / fromJust (lookup n tableCR)


isConsistent :: (Fractional a, Ord a) => a -> Bool
isConsistent cr = cr <= 0.1

processMatrix :: (Show t, Field t) => Matrix t -> IO ()
processMatrix m = do
    let (peval, pevec) = analyzeEigens m
    let npvec = normPrincipalEigenVector pevec
    let n = getSize m
    let ci = consistencyIndex peval n
    let cr = consistencyRatio ci n
    print m
    print npvec
    print $ isConsistent cr

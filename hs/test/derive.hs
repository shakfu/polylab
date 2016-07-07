module Derive where

data D a = D a a deriving (Eq, Show)

instance Num a => Num (D a) where
    D x x' + D y y' = D (x + y) (x' + y')
    D x x' * D y y' = D (x * y) (y' * x + x' * y)
    fromInteger x   = D (fromInteger x) 0
    negate (D x x') = D (negate x) (negate x')
    signum (D x _)  = D (signum x) 0
    abs (D x x')    = D (abs x) (x' * signum x)

instance Fractional x => Fractional (D x) where
    fromRational x  = D (fromRational x) 0
    recip (D x x')  = D (recip x) (x' / sqr x)

sqr :: Num a => a -> a
sqr x = x * x

instance Floating x => Floating (D x) where
    pi              = D pi 0
    exp  (D x x')   = D (exp x) (x' * exp x)
    log  (D x x')   = D (log x) (x' / x)
    sqrt (D x x')   = D (sqrt x) (x' / (2 * sqrt x))
    sin  (D x x')   = D (sin x) (x' * cos x)
    cos  (D x x')   = D (cos x) (x' * (-sin x))
    asin (D x x')   = D (asin x) (x' / sqrt (1 - sqr x))
    acos (D x x')   = D (acos x) (x' / (-sqrt (1 - sqr x)))

    

module Love where

import qualified Data.Map as Map  
import Control.Monad

data Person = Person { 
    firstName :: String, 
    lastName :: String, 
    age :: Int
    } deriving (Show, Eq)

   
data LockerState = Taken | Free deriving (Show, Eq)  

type Code = String  

type LockerMap = Map.Map Int (LockerState, Code) 

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist"
        Just (state, code) -> if state /= Taken
                                    then Right code
                                else Left  $ "Locker " ++ show lockerNumber ++ " is already taken!"  

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]


m1 :: IO ()
m1 = do
    forM_ [1..10] $ \i -> do
        putStrLn $ "i=" ++ show i

x1 :: IO Int
x1 = return 1

m2 = fmap (+5) x1

m3 = liftM (+3) x1

f1 = foldl (+) 0 [1 .. 10]
f2 = foldl (+) 10 [1..10]
sum' xs = foldl (+) 0 xs


-- sieve :: [Int] -> [Int]
-- sieve []     = []
--sieve (p:xs) = p : xs [x | x <- xs, x `mod` p > 0]
primes :: [Integer] -> [Integer]
primes xs = 2 : [x | x <- xs, isprime x]
    where
        isprime x = all (\p -> x `mod` p > 0) (factorsToTry x)
        factorsToTry x = takeWhile (\p -> p*p <= x) primes





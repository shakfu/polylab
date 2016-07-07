module Main where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Batch

qsort :: [Integer] -> [Integer]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

-- test for idempotency 
-- applying a function twice has the same result as applying it only once.
prop_idempotent xs = qsort (qsort xs) == qsort xs

-- test for minimum starting value
-- the first element in a sorted list should always be the smallest element of the input list.
prop_minimum xs = 
    not (null xs) ==> 
        head (qsort xs) == minimum xs

prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum xs =
    not (null xs) ==>
        last (qsort xs) == maximum xs

prop_append xs ys =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

prop_sort_model xs = sort xs == qsort xs

options = TestOptions
      { no_of_tests         = 100
      , length_of_tests     = 1
      , debug_tests         = False }

main = do
    runTests "simple" options
        [ run prop_idempotent
        , run prop_minimum
        , run prop_ordered
        , run prop_permutation
        , run prop_maximum
        , run prop_append
        , run prop_sort_model        
        ]

        

module Test.MySolutions where

import Prelude

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

binomial :: Int -> Int -> Int
binomial n 0 = 1
binomial 0 k = 0
binomial n k
    | n == k = 1
    | n < k  = 0
    | otherwise = let facN = factorial n
                      facK = factorial k
                      facNK = factorial (n - k)
                  in facN / (facK * facNK)

-- n! / k! (n - k)!
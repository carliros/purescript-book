module Test.MySolutions where

import Prelude
import Data.Person(Person)

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

pascal :: Int -> Int -> Int
pascal n 0 = 1
pascal n k
    | n < k  = 0
    | n == k = 1
    | otherwise = factorial n / (factorial k * factorial (n - k))

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

fromSingleton :: String -> Array String -> String
fromSingleton def [] = def
fromSingleton def [s] = s
fromSingleton def _ = def

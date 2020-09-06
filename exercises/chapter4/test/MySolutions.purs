module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concat, filter, length, reverse, sort, (..), (:), head, tail, snoc, foldl)
import Data.Int (even)
import Data.Maybe (Maybe(..), fromJust)
import Test.Examples (factorsV3)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven = even 

countEven :: Array Int -> Int
countEven = length <<< filter isEven

squared :: Array Number -> Array Number
squared = map double
    where double n = n * n

nonNegative :: Number -> Boolean
nonNegative n = n >= 0.0

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter nonNegative

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite = (<$?>) nonNegative

isPrime :: Int -> Boolean
isPrime p = if p == 0 || p == 1
            then false
            else (\n -> n == 1) <<< length $ factorsV3 p

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct l1 l2 = do
    n <- l1
    m <- l2
    pure [n, m]

triples :: Int -> Array (Array Int)
triples n = do
    a <- 1 .. n
    b <- a .. n
    c <- b .. n
    guard $ a * a + b * b == c * c
    pure [a, b, c]

primeListNumber :: Int -> Array Int
primeListNumber n = if n > 1 then filter isPrime (2 .. n) else []

primeFactor :: Int -> Array Int -> Maybe ({a::Int, b::Int})
primeFactor n l = 
    case head l of
        Just m -> if (n `mod` m == 0) 
                  then Just $ {a: m, b: (n / m) }
                  else case tail l of 
                            Just ls -> primeFactor n ls 
                            Nothing -> Nothing  
        Nothing -> Nothing

factorizations :: Int -> Array Int
factorizations n = 
                   let primes = primeListNumber n 
                   in case primeFactor n primes of 
                        Nothing -> []
                        Just {a, b} -> if isPrime b 
                                       then [b, a]
                                       else snoc (factorizations b) a

allTrue :: Array Boolean -> Boolean
allTrue ls = foldl (&&) true ls

fibTailRec :: Int -> Int
fibTailRec n = tailFib n 0
    where tailFib 0 acc = acc
          tailFib 1 acc = acc + 1
          tailFib r acc = tailFib (r - 1) 0 + tailFib (r - 2) 0

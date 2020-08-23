module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concat, filter, length, reverse, sort, (..))
import Data.Int (even)
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

factorizations :: Int -> Array Int
factorizations n = if n>3 then factors else []
    where factors = 
            let list = do
                    a <- 2 .. (n-1)
                    guard $ isPrime a
                    b <- a .. (n - 2)
                    guard $ isPrime b
                    guard $ a * b == n
                    pure [a, b]
            in reverse <<< sort $ concat list

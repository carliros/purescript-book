module Test.MySolutions where

import Prelude

import Data.Array ((:))
import Data.Ord (Ordering(..))

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = let plusSign = if imaginary >= 0.0 then "+" else ""
                                       in show real <> plusSign <> show imaginary <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) = r1 == r2 && i1 == i2

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty n0 arrayN) (NonEmpty m0 arrayM) = n0 == m0 && arrayN == arrayM

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty n arrayN) = show n <> " : " <> show arrayN

instance semiGroupNonEmpty :: Semigroup (NonEmpty a) where 
  append (NonEmpty a arrayA) (NonEmpty b arrayB) = NonEmpty a (arrayA <> ( b : arrayB))

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty a arrayA) = NonEmpty (f a) (map f arrayA)

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq (Infinite) b = case b of 
                       Infinite -> true
                       Finite _ -> false
  eq (Finite n) b = case b of
                      Infinite -> false
                      Finite m -> n == m

instance ordExtended :: Ord a => Ord (Extended a) where 
  compare (Infinite) b = case b of
                            Infinite -> EQ
                            Finite _ -> GT
  compare (Finite n) b = case b of
                            Infinite -> LT
                            Finite m -> compare n m

module Test.MySolutions where

import Prelude

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
    show (Complex { real, imaginary }) = let plusSign = if imaginary >= 0.0 then "+" else ""
                                         in show real <> plusSign <> show imaginary <> "i"

instance eqComplex :: Eq Complex where
    eq (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) = r1 == r2 && i1 == i2

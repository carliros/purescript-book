module Test.MySolutions where

import Prelude

import Global (readFloat)
import Math (e, pi, sqrt)

double :: Number -> Number
double n = n * n

diagonal :: Number -> Number -> Number
diagonal a b = sqrt ( double a + double b)

circleArea :: Number -> Number
circleArea = (*) pi <<< double

addE :: String -> Number
addE = (+) e <<< readFloat

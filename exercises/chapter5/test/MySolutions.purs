module Test.MySolutions where

import Prelude

import Data.Person (Person)
import Data.Picture (Shape(..), Point(..), origin, Picture, intersect, shapeBounds, Bounds, bounds)
import Data.Maybe (Maybe(..))
import Math (pi)
import Data.Foldable (foldl)

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

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle r radio) = Circle origin (radio * 2.0)
doubleScaleAndCenter (Rectangle _ x y) = Rectangle origin (x * 2.0) (y * 2.0)
doubleScaleAndCenter (Text _ str) = Text origin str
doubleScaleAndCenter (Line (Point p1) (Point p2)) 
    = let x1 = p1.x * 2.0
          y1 = p1.y * 2.0
          x2 = p2.x * 2.0
          y2 = p2.y * 2.0
          deltaX = (x2 + x1) / 2.0
          deltaY = (y2 + y1) / 2.0
      in Line (Point {x: x1 - deltaX, y: y1 - deltaY}) (Point {x: x2 - deltaX, y: y2 - deltaY})

shapeText :: Shape -> Maybe String
shapeText (Text _ str) = Just str
shapeText _ = Nothing

area :: Shape -> Number
area (Circle _ r) = r * r * pi
area (Rectangle _ w h) = w * h
area _ = 0.0

data ShapeEx = Clipped Picture Point Number Number
             | ShapeEx Shape

shapeBounds' :: ShapeEx -> Bounds
shapeBounds' (Clipped pics p w h) = intersect (bounds pics) (shapeBounds (Rectangle p w h))
shapeBounds' (ShapeEx s) = shapeBounds s

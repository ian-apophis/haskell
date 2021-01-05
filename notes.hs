
import qualified Data.Map as Map

doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
                         then x
                         else x*2

-- How to use guards + where
-- What even units are these supposed to be?

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "Underweight!"
  | bmi <= 25.0 = "Normal!"
  | bmi <= 30.0 = "Fat!"
  | otherwise = "You're a whale! Go spend some money on mtx!"
  where bmi = weight / height ^ 2

-- List comprehensions?
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs ]
    where bmi weight height = weight / height ^ 2

-- Let bindings
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea

-- 'let' bindings are expressions themselves.
-- 'where' bindings are just syntactic constructs.
-- e.g.
-- 4 * (let a = 9 in a + 1) + 2


-- Case statements
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list"

-- Higher-order functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- null function can be used to quickly check if a list is null
-- let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]

filterEmptyLists :: [[a]] -> [[a]]
filterEmptyLists = filter $ not . null

-- find the sum of all odd squares that are smaller than 10,000

-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x = x : if odd x
                 then chain ((3 * x) + 1)
                 else chain (x `div` 2)

-- lambdas
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- foldl / foldr
-- As is tradition, foldl and foldr's provided functions have their
-- arguments reversed.
myProduct :: Num a => [a] -> a
myProduct = foldl (\acc x -> acc * x) 1

myProduct' :: Num a => [a] -> a
myProduct' = foldr (\x acc -> x * acc) 1

-- scanl/scanr can be used when you need intermediate values from a foldr/foldl

-- $ is function application
-- . is function composition

-- `insertWith` allows you to optionally combine elements into your map!!

{-

Creating a module definition is fairly simple:

ModuleName.hs

module ModuleName
( functionZero
, functionOne
, functionTwo
, functionThree
) where

 /Geometry
    +Sphere.hs
    +Cube.hs
    +Cuboid.hs

module Geometry.Cuboid
( volume
, area
) where

-}

-- ================== DATA TYPES =====================

-- The circle value constructor has 3 parameters. Value constructors are
-- actually functions that ultimately return a value of a data type.
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

{-
Exporting a type is pretty simple:

module Shapes
( Point(..)
, Shape(..)
, surface
) where

...

It would be fine to not export the value constructors, which could
be done by including them without the `(..)`


-}

data Person = Person String String Int Float String String deriving (Show)

flavor :: Person -> String
flavor (Person _ _ _ _ _ f) = f

data PersonRecord = PersonRecord { firstName :: String
                                 , lastName :: String
                                 , age :: Int
                                 -- ...
                                 } deriving (Show)

-- This is just syntactic sugar for creating functions manually as we did in
-- the above case


{-
    Type aliases are also a thing!

    type String = [Char]

-}

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

-- Making typeclass instances
data TrafficLight = Red | Yellow | Green

-- Because Eq is defined in a mutually recursive way, only one of == & /= needs
-- to be defined.
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- The Functor Typeclass
-- Basically for things that can be mapped over.

{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b

fmap takes a function from one type to another, and a functor
applied with one type, and returns a functor applied with another
type.

It's similar to the signature for map:
map :: (a -> b) -> [a] -> [b]
-}

{-

The way Map is a functor probably looks something like this:

instance Functor Map where
    fmap f (Map m) = fromList $ map (\(k, v) -> (k, f v)) $ toList m
-}

-- Tomorrow!
-- Input and Output!!

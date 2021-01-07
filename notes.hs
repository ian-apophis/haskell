
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

-- Types and Typeclasses DAY TWO

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

-- ============= RECORD SYNTAX ====================

-- The info we want to store is:
-- first name, last name, age, height, phone number, and favourite ice-cream
-- flavour.
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

-- ======================= TYPE PARAMETERS ===============================

-- A value constructor can take some values parameters and then produce a new
-- value. In a similar manner, _type constructors_ can take types as parameters
-- to produce new types.
--
-- e.g.
-- data Maybe a = Nothing | Just a
--
-- Because there is a type parameter involved, we call Maybe a type
-- constructor. No value can have a type of just Maybe, because that's not a
-- concrete type (*), it's a type constructor.
--
--
-- Typeclass constraints are mentioned in the Learn you a Haskell, but have
-- been removed from the language.

-- ======================== DERIVED INSTANCES =================================
--
-- Haskell can automatically make types instances of Eq, Ord, Enum, Bounded,
-- Show, Read using the 'deriving' keyword.
--

data Day = Montag | Dienstag | Mittwoch | Donnerstand | Freitag | Samstag | Sonntag
           deriving(Eq, Ord, Show, Read, Bounded, Enum)

-- When you do something like:
-- read "Samstag" :: Day, what the heck is going on?
--
-- The type of read is:
--
-- read :: Read a => String -> a
--
-- The kind of Read is:
--
-- Read :: * -> Constraint
--
-- :: seems to be some sort of type declaration thingey


{-
    Type aliases are also a thing!

    type String = [Char]

-}

-- You can partially apply type parameters and get new type constructors.
-- e.g.
-- type IntMap = Map.Map Int
-- This one cannot be done though, because Map.Map's Data constructor is not
-- in scope


-- Lets look at more cool data types:
-- data Either a b = Left a | Right b deriving(Eq, Ord, Read, Show)

-- Lets do a map example!
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
      Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
      Just (state, code) -> if state /= Taken
                               then Right code
                               else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

-- ======================== RECURSIVE DATA STRUCTURES ==========================

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

--
-- You can't do something like:
--
-- instance Eq Maybe where
--  ...
-- But you can do something like
--
-- instance (Eq m) => Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ = False
--
-- This instance declaration says that:
-- We want all types of the form `Maybe m` to be part of the `Eq` typeclass,
-- but only those types where the `m` is also a part of `Eq`.
--

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

-- The Functor Typeclass
-- Basically for things that can be mapped over.

{-

class Functor f where
    fmap :: (a -> b) -> f a -> f b

In the definitions of typeclasses we've seen so far, the type variable was a
concrete type. e.g.
(==) :: (Eq a) => a -> a -> Bool

But now, the `f` is not a concrete type, but a type constructor that takes one
parameter.

fmap takes a function from one type to another, and a functor applied with one
type, and returns a functor applied with another type.

It's similar to the signature for map:
map :: (a -> b) -> [a] -> [b]

Indeed, map is fmap that works only on lists!!
-}

-- Lets try to write an instance of Functor for Tree.
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

{-

The way Map is a functor probably looks something like this:

instance Functor Map.Map where
    fmap f Map.Empty = Map.Empty
    fmap f (Map.Map m) = fromList $ map (\(k, v) -> (k, f v)) $ toList m

First, the Map is converted to a list, then map a lambda transforming all the
key value pairs into the new type, then construct a new map from the list.

-}

-- ===================================== KINDS =================================

-- use :k to lookup the kind.
-- * is a concrete type
--
-- Maybe is a type constructor, so its kind is:
-- * -> *
--
-- That is, it takes a concrete type and returns a concrete type
--
-- Maybe Int would have kind `*`
--

-- Type foo time
class Tofu t where
    tofu :: j a -> t a j

-- How the heck would we instance this?
-- a is a *, so j is * -> *, therefore t must be `* -> (* -> *) -> *`

data Frank a b = Frank {frankField :: b a} deriving (Show)

-- Frank is a type constructor that takes a concrete type, and a type constructor
-- `* -> (* -> *) -> *` exactly.

instance Tofu Frank where
    tofu = Frank

-- This is a bit brain melty, but we shall move on to Input & Output!!

-- ====================== INPUT AND OUTPUT ====================================

-- putStrLn takes a string and returns an I/O action that has a result of type
-- ()
--
-- main = do
--      ...
--
-- is how you can make a runnable program. main always has a type signature of
-- `main :: IO _something_`
--
-- name <- getLine
--
-- :t getLine is `getLine :: IO String`, e.g. an I/O action that returns a
-- string.
--
-- The <- construct is used to retrieve data from an I/O action.
--
-- In a `do` block, the last action cannot be bound to a name!
--
-- `return` in haskell actually creates an I/O action out of a pure value
-- 
-- The `when` function is found in Control.Monad.
-- It takes a boolean value and an I/O action
--
-- sequence
-- What the heck?
--
-- Tomorrow, go over I/O actions again! Especially:
-- - sequence
-- - mapM
-- - forM
-- - when
-- - forever

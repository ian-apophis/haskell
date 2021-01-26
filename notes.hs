
import qualified Data.Map as Map
import Data.List.Split
import Control.Monad
import Data.Monoid
import Control.Applicative
import System.Random

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

-- This list comprehension uses a let binding with no "in" and uses the defined
-- variable both before _and_ after the let binding.
calcBMIs :: (RealFloat a) => [(a, a)] -> [a]
calcBMIs xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25]

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

-- Making typeclass instances. Typeclasses are like interfaces
data TrafficLight = Red | Yellow | Green

-- Because Eq is defined in a mutually recursive way, only one of == & /= needs
-- to be defined.
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- class is for defining new typeclasses
-- instance is for making our types instances of typeclasses.

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

--
-- You can't do something like:
--
-- instance Eq Maybe where
--  ...
-- But you can do something like:
--
-- instance (Eq m) => Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ = False
--
-- That is, we must add a class constraint. We are using `==` so we must
-- guarantee that a class is a member of the Eq typeclass
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

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

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
-- Similar to list comprehensions, a let binding doesn't need an "in"
--
-- In a `do` block, the last action cannot be bound to a name!
-- The following program is not valid:
--
-- main = do
--     illegal <- putStrLn "Wow!"
--
-- `return` in haskell actually creates an I/O action out of a pure value. It's
-- sort-of like the opposite of `<-`.
example = do
    a <- return "Hell"
    b <- return "Yeah!"
    putStrLn $ a ++ " " ++ b
--
-- They're mostly used to create an I/O action that doesn't do anything
--
example2 = do
    c <- getChar
    if c /= ' '
       then do
           putChar c
           example2
        else return ()
--
-- The `when` function is found in Control.Monad.
-- It takes a boolean value and an I/O action and is sort-of syntactic sugar
-- for the `if COND then ACTION else return ()` construct.
--
example3 = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        example3

-- sequence takes a list of I/O actions and returns an I/O action that will
-- perform those actions one after another. The result contained in that I/O
-- action will be a list of the results of all the I/O actions that were
-- performed.
-- sequence :: [IO a] -> IO [a]
example4 = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

-- is the same as
example5 = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

-- A common pattern is when we map functions like `print` or `putStrLn` over lists.
-- sequence (map print [1,2,3,4,5])

-- File IO is fun. See todo.hs

-- ========================== RANDOMNESS =======================================

-- Requires a whole separate package on ubuntu 20.04

-- random (mkStdGen 100) :: (Int, StdGen)
--
-- take 5 $ randoms (mkStdGen 11) :: [Int]
--
-- randomR (1, 6) (mkStdGen 359353)
--
-- take 10 $ randomRs ('a', 'z') (mkStdGen 3) :: [Char]
--
-- This has the issue of needing to provide a random seed, which isn't good.
-- System.Random offers the getStdGen I/O action. This asks the system for a
-- good random number generator and stores that in a global generator.

example6 = do
    replicateM_ 5 $ do
        gen <- newStdGen
        putStrLn $ take 20 (randomRs ('a', 'z') gen)

-- newStdGen splits the current random generator into two generators.

-- Bytestrings are a thing and are useful for processing files.

-- Exceptions!
-- import Control.Exception(catch)
-- catch :: IO a -> (IOError -> IO a) -> IO a

solveRPN :: (Num a, Read a) => String -> a
solveRPN expression = head (foldl foldingFunction [] (words expression))
    where foldingFunction (x:y:ys) "*" = (x*y):ys
          foldingFunction (x:y:ys) "+" = (x+y):ys
          foldingFunction (x:y:ys) "-" = (y-x):ys
          foldingFunction xs numberString = read numberString:xs

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

uncurry3 f [a,b,c] = f a b c
heathrowToLondon :: RoadSystem
heathrowToLondon = map (uncurry3 Section) $ chunksOf 3 $ map read . words $ "50 10 30 5 90 20 40 2 25 10 8 0"

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        forwardPriceToA = priceA + a
        crossPriceToA = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB = priceA + a + c
        newPathToA = if forwardPriceToA <= crossPriceToA
                        then (A,a):pathA
                        else (C,c):(B,b):pathB
        newPathToB = if forwardPriceToB <= crossPriceToB
                        then (B,b):pathB
                        else (C,c):(A,a):pathA
    in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath rs =
    let (pa,pb) = foldl roadStep ([],[]) rs
        ca = sum $map snd pa
        cb = sum $map snd pb
    in reverse $ if ca <= cb then pa else pb

-- Functors, Applicative Functors and Monoids
--
-- (->) r is a functor too!
-- `r -> a` can be rewritten as `(->) r a` much like `2 + 3` can be re-written as
-- `(+ 2 3)`
--
-- -> is a type constructor that takes two types
--
-- Does that mean the kind of of (->) is `* -> * -> *`? IT IS!!
-- :k (->) => `* -> * -> *`
--
-- fmap just for ((->) r) would look like:
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)
--
-- How are functions functors?
-- instance Functor ((->) r) where
--     fmap = (.)
-- It was function composition this whole time!!
--
-- fmap can be thought of a function that takes a function
-- and returns a function that takes a functor and returns a functor.
--
-- It takes an `a->b` function and returns a function `f a -> f b`
--
-- This is called _lifting_ a function
--
-- We can also look at functors as things that output values in a context.
-- e.g.
-- Just 3 outputs 3 in the maybe context
-- [1,2,3] outputs three values, in the context that there may be multiple or
-- no values.
--
-- Mapping over functors is like attaching a transformation to the output of
-- the functor that changes the value.
--
--

-- ====================== APPLICATIVE FUNCTORS ================================
--
-- fmap (*) (Just 3)
-- Num a => Maybe (a->a)
--
-- You can map functions that are partially applied over functors, which makes
-- sense.
--
-- Instead of mapping functions of "lists" of "values" you can map values over
-- "lists" of "functions"
--
-- Using standard fmap you cannot however map a function that's inside a
-- functor over another functor.
--
-- Applicative Typeclass in Control.Applicative
-- It defines two methods:
    {-
       class (Functor f) => Applicative f where
           pure :: a-> fa
           (<*>) :: f(a->b) -> f a -> f b
    -}

-- pure should take a value of any type and return an applicative functor with
-- that value inside of it.
--
-- <*> is sort of like a beefed up fmap. Whereas fmap takes a function and a
-- functor and applies the function inside the functor, <*> takes a functor
-- that has a function in it and another functor and sort-of extracts the
-- function from the first functor and then maps it over the second one.
--
-- let a = Just (*3)
-- a <*> (Just 4)
--
-- let a = replicate 3 (*3)
-- a <*> [1,2,3,4]
-- [3,6,9,12,3,6,9,12,3,6,9,12]
--
-- list implements <*> with a cross-product like thing
--
-- Using haskell's strong typing you can use pure to coerce functions into the
-- correct functor to be <*>'d over something.
--
-- pure (*3) <*> [1,2,3,4]
-- [3,6,9,12]
-- pure (+) <*> Just 3 <*> Just 5
-- 8
--
-- Or better to say, you can use pure to take a function that expects functions
-- that aren't wrapped in functors and use that to operate on values in functor
-- contexts. This is sort-of a reverse monad!
--
-- Control.Applicative also exports a function <$>, which is infix map
-- The following 3 definitions are identical!

fancyfunc1 l = fmap (*3) l
fancyfunc2 l = pure (*3) <*> l
fancyfunc3 l = (*3) <$> l

example8 = do
    let a = take 10 $ fancyfunc1 [1..]
    print a
    let a = take 10 $ fancyfunc2 [1..]
    print a
    let a = take 10 $ fancyfunc3 [1..]
    print a

-- IO is also an applicative functor
example9 = (++) <$> getLine <*> getLine

--
-- Lets look at how ((->) r) is Applicative
    {-
        instance Applicative ((->) r) where
            -- id-like function
            pure x = (\_ -> x)
            f <*> g = \x -> f x (g x)
    -}
-- f <*> g takes a function (a->a->a) and a function (a->a) and makes
-- it into a function of (a->a) it also has the effect of doubling
-- the argument!
example10 = (replicate) <$> (*3) <*> (*(-1))
-- This function fmaps replicate over (*3) to get a function
-- that replicates 3x as many as you want. Then when we use
-- <*> (*(-1)) distributes the argument.

-- This function calls + on the eventual results of (+3) and (*100).
-- Rather than simply composinge the (+3) and (*100) that one might expect
-- it first fmaps (+) over (+3) so you get an applicative functor.
-- :t (+) <$> (+3)
-- (+) <$> (+3) :: Num a => a -> a -> a
-- :t (+) <$> (+3) <*> (*100)
-- (+) <$> (+3) <*> (*100) :: Num b => b -> b
-- Why does it not take 2 arguments?
simpleAppFunc = (+) <$> (+3) <*> (*100)

-- This function will call `(\x y z -> x:y:z:[])` with the eventual
-- results of (+3) (*2) and (/2)
crazyAppFunc = (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2)

-- ((->) r) IS AN APPLICATIVE FUNCTOR THAT ACTS LIKE DISTRIBUTING AN ARGUMENT

-- ZipLists are a typeclass that is an instance of applicative. (This implies
-- it's also a functor). When used with the <*> operator, they behave like
-- zipWith ($)
--
-- ZipLists allow you to zip many more lists together without requiring
-- special functions. They also allow you to make your functions on
-- the spot rather than having to prepare them separately.
-- getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
--
-- (,) is a function that takes 2 arguments and makes a tuple out of them
--
-- liftA2 has the type:
-- (a->b->c)->(f a -> f b -> f c)
--
-- It takes a normal binary function and promotes it to a function that
-- operates on two functors
-- liftA2 (,) (Just 3) (Just 4)
-- Just (3,4)
--
-- Sequence is a function that takes a list of applicatives and
-- returns an applicative with a list.
--
-- sequence [Just 1, Just 2, Just 3, Just 4]
-- Just [1,2,3,4]
--
-- Why does
-- :t sequence [(*3), (*5), (*10)]
-- sequence [(*3), (*5), (*10)] :: Num a => a -> [a]
-- In this case the applicative is ((->) r) so it does
-- the whole (:) <$> x <*> sequence xs
--
-- The actual implementation is `mapM id`
--
-- =========================== MONOIDS ========================================
--
-- VERY IMPORTANT: Monoids are sort of like socks.
--
-- an associative binary function for which there exists a value which acts as
-- an identity w.r.t. that function.
--
-- Monoids are useful because they allow you to define how to do things like
-- folding/collecting on a type.
--
-- The standard fold* family of functions only operate on lists, but the
-- versions in the Foldable module can be used on any type that is foldable.
--

-- getAny $ F.foldMap (\x -> Any $ x == 3) testTree
--
-- (\x -> Any $ x == 3) takes values and produces the monoid (Any) which can
-- then be `mappend`-ed to all the other elements in the tree.

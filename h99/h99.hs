
-- Problem 1, last element of a list
myLast = last

-- Problem 2, 2nd to last element
myButLast = last . init

-- Problem 3, K'th element of a list
elementAt :: [a] -> Int -> a
elementAt x y = x !! (y - 1)

-- Problem 4, Find the number of elements
myLength :: [a] -> Int
myLength = length

-- Problem 5, Reverse a list
myReverse = reverse

-- Problem 6, Check for palindrome
isPalindrome list = reverse list == list

-- Problem 7, flatten nested list structure
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a)        = [a]
flatten (List (x:xs))   = flatten x ++ flatten (List xs)
flatten (List [])       = []


-- Problem 8, eliminate consecutive duplicates

compress' :: Eq a => a -> [a] -> [a]
compress' _ [] = []
compress' rej (x:xs) = if rej == x
                      then compress' rej xs
                      else x : compress' x xs

compress :: Eq a => [a] -> [a]
compress []     = []
compress (x:xs) = x : compress' x xs

-- Problem 9, pack consecutive duplicates into sublists

pack' :: Eq a => [a] -> [a] -> [[a]]
pack' [] [] = []
pack' l [] = [l]
pack' [] (x:xs) = pack' [x] xs
pack' l (x:xs) = if head l == x
                    then pack' (x:l) xs
                    else l : pack' [x] xs

pack :: Eq a => [a] -> [[a]]
pack l = pack' [] l

-- Problem 10, run length encoding a list
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode l = let tuplize all@(x:xs) = (length all, x) in map tuplize $ pack l

-- Problem 11, modified run-length encoding
data Encoded a = Multiple Int a | Single a
    deriving (Show)

encodeElement 1 y = Single y
encodeElement n y = Multiple n y

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified [] = []
encodeModified l = let tuplize all@(x:xs) = encodeElement (length all) x
                    in map tuplize $ pack l

-- Problem 12, decode a run-length encoded list

decodeModified :: Eq a => [Encoded a] -> [a]
decodeModified [] = []
decodeModified ((Multiple val c):xs) = replicate val c ++ decodeModified xs
decodeModified ((Single c):xs) = [c] ++ decodeModified xs

-- Problem 13, run-length encoding of a list
-- One solution does a foldr which allows the value being modified to remain at
-- the head of the list.
encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect = map (\(x,y) -> encodeElement x y) . foldr encodel' []
    where
        encodel' x [] = [(1,x)]
        encodel' x (y@((b,c):ys))
            | x == c    = (b+1, c):ys
            | otherwise = (1, x):y  -- val doesn't match, prepend

-- Problem 14, Duplicate the elements of a list
dupli :: [a] -> [a]
dupli = foldr helper []
    where
        helper x acc = x:x:acc

-- Problem 15, Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli list val = foldr (helper val) [] list
    where
        helper v x acc = replicate v x ++ acc

-- Problem 16, Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery lst v = map (snd) $ filter (\(x, y) -> x `mod` v /= 0) $ zip [1..] lst

-- Problem 17, Split a list into two parts
split :: [a] -> Int -> ([a], [a])
split lst v = (take v lst, drop v lst)

-- Problem 18, Extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice lst start end = take (end - start + 1) $ drop (start - 1) lst

-- Problem 19, Rotate a list N places to the left
rotate :: [a] -> Int -> [a]
rotate lst v
  | v == 0 || abs v >= length lst = lst
  | v > 0 = drop v lst ++ take v lst
  | v < 0 = drop rst lst ++ take rst lst
            where rst = length lst + v

-- Problem 20, Remove the K'th element from a list
removeAt :: Int -> [a] -> (a, [a])
removeAt v lst = (lst !! (v - 1), take (v-1) lst ++ drop v lst)

-- Problem 21, Insert an element at a given position into a list
insertAt :: a -> [a] -> Int -> [a]
insertAt ins lst pos = take (pos-1) lst ++ [ins] ++ drop (pos-1) lst

-- Problem 22, Create a list containing all integers within a given range
range :: Integer -> Integer -> [Integer]
range x y
  | x <= y = [x..y]
  | otherwise = [x, x-1..y]

-- From here on random numbers seem to be pretty important

import System.Random

-- This one seems straightforward, but using random seems to have strange
-- side-effects in Haskell. This probably has something to do with the fact
-- that generating a value causes the internal RNG state to cycle.
rnd_select :: [a] -> Int -> [a]
rnd_select [] _ = []
rnd_select xs n = take n xs

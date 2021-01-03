
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
encode (x:xs) = [(1, x)]


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

-- scanl/scanr can be used when you need intermediate values from a foldr/foldl

-- $ is function application
-- . is function composition

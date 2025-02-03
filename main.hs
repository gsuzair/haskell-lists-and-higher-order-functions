-- 5.1
-- 1.
main :: IO ()
main = do
  let result = listEvens 13 21
  print result

listEvens :: Int -> Int -> [Int]
listEvens x y = if y < x then [] 
                else [k | k <- [y, y-1 .. x], even k]


-- 2.
main :: IO ()
main = do
  let result = pythagoreanTriples 13
  print result

pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = 
  [(a, b, c) | c <- [1..n], 
               b <- [1..c], 
               a <- [1..b], 
               a^2 + b^2 == c^2]

-- 3. 
main :: IO ()
main = do
  let result = addPairwise [-7, -8, -9] [10, 11, 12]
  print result

addPairwise :: [Int] -> [Int] -> [Int]
addPairwise xs ys =  zipWith (+) xs ys

-- 4.
main :: IO ()
main = do
  let result = subList [-7, -8, -9 , 1, 5, 8] (2,4)
  print result

subList :: [Int] -> (Int, Int) -> [Int]
subList xs (i, j) =  [xs !! n | n <- [i..j]]


main :: IO ()
main = do
  let result = subList [1..10000000] (9999990,9999999)
  print result

subList :: [Int] -> (Int, Int) -> [Int]
subList xs (i, j) =  [xs !! n | n <- [i..j]]

subList' :: [a] -> (Int, Int) -> [a]
subList'    xs     (i  , j  ) =  take (j-i+1) (drop i xs)

-- subList: accessing an element using xs !! n takes O(n) time because Haskell lists are linked lists, and indexing requires traversing the list to the n-th element.
-- If the range [i..j] has m elements, this results in O(m * i) complexity, where m = j - i + 1.
-- This is slow for large lists, especially when i or j is large, because it repeatedly traverses the list to access elements.
-- subList' is faster because drop i takes O(i) time to skip the first i elements.
-- take m takes O(m) time to construct the sublist of size m = j - i + 1.
-- The total complexity is O(i + m), which is more efficient than subList.

-- 5.
main :: IO ()
main = do
  let result = together [1, 2, 3] 
  print result

together :: [a] -> [(a, a)]
together xs 
        | length xs < 2 = []
        | otherwise = zip xs(tail xs)

-- tail skips the first element of the list
-- zip creates pairs of the list


                      
-- 5.2
-- 1.
-- elem x xs
-- elem 3 [1, 2, 3, 4, 5]
-- Result: True

main :: IO ()
main = do
  let result = contains [1, 2, 3] 4 
  print result

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False  -- for base case, empty list cant return true
contains (x:xs) a
        | x == a = True
        | otherwise = contains xs a -- calls the function again without the first element which is x

contains :: Eq a => [a] -> a -> Bool
contains xs y = any (== y) xs

-- Explanation:
-- The any function takes a predicate (a function that returns Bool) and a list, and it returns True if any element in the list satisfies the predicate.
-- The predicate (== y) is an operator section, meaning it checks if an element is equal to y.
-- any (== y) xs checks if there is any element in xs equal to y.

-- 2.
main :: IO ()
main = do
  let result = nth 2 [1, 2, 3]
  print result

nth :: Int -> [a] -> Maybe a
nth n (a:as)
        | length as < n = Nothing
        | n < 0 = Nothing
        | n == 0 = Just a
        | otherwise = nth(n-1) as

-- 3.
remove :: Eq a => a -> [a] ->  [a]
remove            a    (a':as) | a == a'   =  remove a as
                               | otherwise =  a' : remove a as
remove            _    []  =   []

remove'' :: Int -> [Int] -> [Int]
remove''    a      ls     =  [ a' | a' <- ls, a' /= a]

remove' :: Eq a => a -> [a] ->  [a]
remove'            a =  filter (/= a)

-- 4.
main :: IO ()
main = do
  let result = substitute 5 10 [1, 5, 2, 5, 3, 5]
  print result

substitute :: Eq a => a -> a -> [a] -> [a]
substitute            _    _    []  =  []
substitute            x    y    (a:xs) | x == a   = y  : substitute x y xs
                                      | otherwise = a : substitute x y xs
                
        
-- x == x': This guard checks if the current element (x') is equal to the element to be substituted (x). If true, it replaces x' with y and recurses on the rest of the list (xs).
-- otherwise: This is a catch-all case (equivalent to True in Haskell). If x' is not equal to x, the element x' remains unchanged, and the function recurses on the rest of the list.

--5.3
--1
--part one
main :: IO ()
main = do
  let result = addTime (3,11) (5,18)
  print result

type Time = (Int, Int)
addTime :: Time -> Time -> Time
addTime (minute1, second1) (minute2, second2) = 
      (totalMinutes, remainingSeconds)
      where
        totalSeconds = second1 + second2
        extraMinutes = totalSeconds `div` 60
        remainingSeconds = totalSeconds `mod` 60
        totalMinutes = minute1 + minute2 + extraMinutes

-- part two
main :: IO ()
main = do
  let result = sumTime [(3, 50), (5, 20), (2, 45), (4, 30)]
  print result

type Time = (Int, Int)
type TimeList = [(Int, Int)]

addTime :: Time -> Time -> Time
addTime (minute1, second1) (minute2, second2) = 
      (totalMinutes, remainingSeconds)
      where
        totalSeconds = second1 + second2
        extraMinutes = totalSeconds `div` 60
        remainingSeconds = totalSeconds `mod` 60
        totalMinutes = minute1 + minute2 + extraMinutes
        
sumTime :: TimeList -> Time
sumTime [] = (0, 0)
sumTime (t:ts) = addTime t (sumTime ts)

-- 5.4.1 A function addDashes that takes a list of strings and adds "comment dashes" on the front of each
-- one. Use in your implementation a suitable mapping function supplied by Haskell (e.g. all, map,
-- filter, etc.), operator sections and Currying to arrive at a very succinct solution.

-- main :: IO ()
-- main = do
--   let result = addDashes ["a", "b", "c"]
--   print result
  
addDashes :: [String] -> [String]
addDashes [] = []  -- Base case to handle empty list
addDashes (s:st) = addActualDashes s : addDashes st

addActualDashes :: String -> String
addActualDashes s = "--" ++ s  -- Use `++` for string concatenation

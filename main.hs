-- 5.1.1  A function listEvens that takes two numbers x and y with y  x and returns a list containing
-- the even numbers between x and y in decreasing order. For this, use list comprehensions and the
-- syntactic sugar for arithmetic sequences from the lecture. (It also works for variables.) Recall that
-- an arithmetic sequence is a sequence of numbers where the difference between adjacent elements is
-- always the same. Note also that if y < x the function must return the empty list. For example:
-- listEvens 13 29 ==> [28, 26, 24, 22, 20, 18, 16, 14]
-- listEvens 20 31 ==> [30, 28, 26, 24, 22, 20]
-- listEvens 31 20 ==> [] (in this case y < x)
-- listEvens (-29) (-22) ==> [-22, -24, -26, -28]
main :: IO ()
main = do
  let result = listEvens 13 21
  print result

listEvens :: Int -> Int -> [Int]
listEvens x y = if y < x then []
                else [k | k <- [y, y-1 .. x], even k]


-- 5.2.2  A function pythagoreanTriples that takes a number n >= 0 and returns the list of Pythagorean
-- triples smaller than n, i.e triples (a, b, c) with a <= b <= c <= n and the square of c equalling the
-- sum of the squares of a and b. For this exercise, we will use the type Triple = (Int, Int, Int)
-- and in this form the type of pythagoreanTriples must be Int -> [Triple].
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

-- 5.2.3 Use Hoogle to find out about the function zipWith, and use it to write a function addPairwise
-- which adds corresponding elements of two lists of Int, and drops any element that does not have
-- a pair on the second list. For example:
-- addPairwise [1, 2] [3, 4, 5] ==> [4, 6]
-- addPairwise [] [1, 2, 3] ==> []
-- addPairwise [-7, -8, -9] [10, 11, 12] ==> [3, 3, 3] 
main :: IO ()
main = do
  let result = addPairwise [-7, -8, -9] [10, 11, 12]
  print result

addPairwise :: [Int] -> [Int] -> [Int]
addPairwise xs ys =  zipWith (+) xs ys

-- 5.2.4 A function subList that takes a list xs and a pair of indices (i, j) and returns the list containing
-- the ith, the i + 1th etc. until the jth element as long as 0 <= i <= j <= length(xs − 1). Use list
-- comprehension together with the operator !! for this exercise.
-- subList [0, 1, 2, 3, 4, 5] (2, 4) ==> [2, 3, 4]
-- subList [’a’, ’b’, ’c’, ’d’, ’e’] (1, 3) ==> [’b’, ’c’, ’d’]
-- Now present a function subList’ that does exactly the same, but in this case you must program
-- it using the predefined Haskell functions take and drop.
-- Which is the better solution? Compare them by running:
-- subList [1..10000000] (9999990,9999999)
-- subList’ [1..10000000] (9999990,9999999)
-- Why is one of them much faster?

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

-- 5.1.5 A function together that takes a list xs with length xs  2 and pairs up adjacent elements of
-- the list. For example:
-- together [1, 2] ==> [(1, 2)]
-- together [’a’, ’b’, ’c’] ==> [(’a’, ’b’), (’b’, ’c’)]
-- together ["compass", "name", "line"]
-- ==> [("compass", "name"), ("name", "line")]

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



-- 5.2 1 A function contains :: Eq a => [a] -> a -> Bool whose purpose is to tell us whether a given
-- item is present in a given list (this has the same functionality as the predefined function elem).
-- Implement it two times, once using pattern matching and recursion on the list, once using any and
-- operator sections
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

-- 5.2.2 The type Maybe a, which will return in later lectures, can be inhabited either by the value Nothing,
-- representing an error or a missing value, or Just x, where x is of type a.
-- Write function nth :: Int -> [a] -> Maybe a that returns the n
-- th item in a list counting from
-- 0 (similar to the operator !!). If n is bigger than the length of the list, or negative, nth should return
-- Nothing. For example, nth 2 [12, 13, 14, 15] ==> Just 14, but nth 3 ["foo", "bar"] ==>
-- Nothing. You will need pattern-matching and guards for this.

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

-- 5.2.3 A function remove that removes all occurrences of a given item in a list. For example:
-- remove 5 [1, 5, 2, 5, 3] ==> [1, 2, 3]
-- remove 8 [6, 7, 10] ==> [6, 7, 10]
-- Solve this problem two times: First, use pattern-matching and guards. Second, use filter. Which
-- solution is more readable? Which one is easier to write without errors?

remove :: Eq a => a -> [a] ->  [a]
remove            a    (a':as) | a == a'   =  remove a as
                               | otherwise =  a' : remove a as
remove            _    []  =   []

remove'' :: Int -> [Int] -> [Int]
remove''    a      ls     =  [ a' | a' <- ls, a' /= a]

remove' :: Eq a => a -> [a] ->  [a]
remove'            a =  filter (/= a)

-- 5.2.4 A function substitute :: Eq a => a -> a -> [a] -> [a] that substitutes all occurrences of
-- a given item for another. For example:
-- substitute 5 10 [1, 5, 2, 5, 3, 5] ==> [1, 10, 2, 10, 3, 10]
-- substitute "this" "that" ["this", "function", "this"]
-- ==> ["that", "function", "that"]

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

--5.3.1 Adding time: Suppose we want to find the total amount of playing time of a CD containing
-- several songs. For this consider that every song is measured in minutes and seconds in the usual
-- way (i.e. every minute has 60 seconds, and every hour has 60 minutes). In this form, for example,
-- a CD containing three songs of length 5
-- 0 1800 (read as 5 minutes and 18 seconds), 3
-- 0 2700 and 3
-- 0 2500
-- respectively, will have a total playing time of 120 1000. Write a Haskell function for adding various
-- times expressed in a list of minutes and seconds, where each individual time is represented by a
-- tuple.
-- Hint: First, write a function to add two times. Then write a recursive function to sum over a list
-- of times.
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

main :: IO ()
main = do
  let result = addDashes ["a", "b", "c"]
  print result

addDashes :: [String] -> [String]
addDashes [] = []  -- Base case to handle empty list
addDashes (s:st) = addActualDashes s : addDashes st

addActualDashes :: String -> String
addActualDashes s = "--" ++ s  -- Use `++` for string concatenation

--with map:
main :: IO ()
main = do
  let result = addDashes ["a", "b", "c"]
  print result
  
addDashes :: [String] -> [String]
addDashes [] = []  -- Base case to handle empty list
addDashes st = map addActualDashes st

addActualDashes :: String -> String
addActualDashes s = "--" ++ s  -- Use `++` for string concatenation

-- 5.4.2 A function swapPairs that takes a list of pairs of any type and returns a list in which the pairs are
-- swapped. Use for this anonymous functions (lambdas). For example:
-- swapPairs [("two", "one"), ("bye", "hi")] ==> [("one", "two"), ("hi", bye")]
-- swapPairs [(4, 5), (10, 9), (12, 3)] ==> [(5, 4), (9, 10), (3, 12)]
-- Solve this problem two times, once using the predefined Haskell function map, once using list
-- comprehensions.

main :: IO ()
main = do
  let result = swapPairs [(4, 5), (10, 9), (12, 3)]
  print result
  
swapPairs :: [(a, b)] -> [(b, a)]
swapPairs [] = []  -- Base case for an empty list
swapPairs st = map swapPairing st

swapPairing :: (a, b) -> (b, a)
swapPairing (a, b) = (b, a)

--with lambda function
swapPairs' :: [(a,b)] -> [(b,a)]
swapPairs' = map $ \(a,b) -> (b,a)
-- with list comprehension
swapPairs'' :: [(a, b)] -> [(b, a)]
swapPairs'' [] = []  -- Base case for an empty list
swapPairs'' lst = [(b,a) | (a,b) <- lst]





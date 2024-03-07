module Test where

{-
  ! STANDAR
  null list

  head-tail
  init-last

  -- any itu equals to primitive datatype (Int, Char, Bool, Float)

  elemen : list
  any : [any]

  list ++ list
  [any] : [any]

  ! USEFUL
  >> 3 `elem` [1,2,3]
  True
  >> 3 `notElem` [1,2,3]
  False
  
  >> all function [array]
  -- Determines whether all elements of the structure satisfy the predicate which is returned by the function
  
  >> filter function [2..n]
  -- filter , applied to a predicate and a list, returns the list of those elements that satisfy the function
-}

isMenaik :: [Int] -> Bool
isMenaik li
  | length li <= 1 = True
  | head li < head (tail li) = isMenaik (tail li)
  | otherwise = False

subString :: [Char]-> Int -> Int -> [Char]
subString lc start end
  | end < 0 = ""
  | start <= 0 = head lc : subString (tail lc) (start-1) (end-1)
  | otherwise = subString (tail lc) (start-1) (end-1)


twoSum :: [Int]-> Int -> Bool
twoSum li n
  | length li <= 1 = False
  | head li + head (tail li) == n = True
  | otherwise = twoSum (tail li) n || twoSum (head li : tail (tail li)) n
{-
  `elem`
  kalau pasangannya merupakan elemen dari array, berarti dapet jawabannya

  -- lakukan loop untuk cek satu per satu kalau ada pasangannya

-- twoSum :: [Int]-> Int -> Bool
-- twoSum li n
--   | length li <= 1 = False
--   | (n - head li) `elem` tail li = True
--   | otherwise = twoSum (tail li) n
-}

prefixSum :: [Int]-> [Int]
prefixSum li
  | length li <= 1 = [head li]
  | otherwise = head li : prefixSum (head li + head (tail li):tail (tail li))

isSmallestInteger :: Int-> [Int] -> Bool
isSmallestInteger n li
  | length li <= 1 = True
  | n <= head li = isSmallestInteger n (tail li)
  | otherwise = False

insertionSort :: [Int] -> [Int]
insertionSort li
  | length li <= 1 = li
  | isSmallestInteger (head li) (tail li) = head li : insertionSort (tail li)
  | otherwise = insertionSort (tail li ++ [head li])

isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False
  | otherwise = all (\x -> n `mod` x /= 0) [2..(n-1)]

goldbach :: Int -> [(Int, Int)]
goldbach n = pairs
  where
    primes = filter isPrime [2..n]
    pairs  = [(x, y) | x <- primes, y <- primes, x + y == n, x <= y]

isPalindrome :: [Char]-> Bool
isPalindrome lc
  | length (tail lc) <= 1 = True
  | head lc == last (tail lc) = isPalindrome (init (tail lc))
  | otherwise = False

gabungList :: [Int] -> [Int] -> [Int]
gabungList l1 l2
  | null l1 = l2
  | null l2 = l1
  | head l1 <= head l2 = head l1 : gabungList (tail l1) l2
  | otherwise = head l2 : gabungList l1 (tail l2)

isNumOn :: Int ->[Int]->Bool
isNumOn n l2
  | null l2 = False
  | n == head l2 = True
  | otherwise = isNumOn n (tail l2)

setOperation :: [Int] -> [Int]-> Char -> [Int]
setOperation li la c
  | null li && null la = []
  | c == 'U' && null li = insertionSort la
  | c == 'U' && not (isNumOn (head li) la) = setOperation (tail li) (la ++ [head li]) c
  | c == 'U' && isNumOn (head li) la = setOperation (tail li) la c
  | null li || null la = []
  | c == 'I' && isNumOn (head li) la = head li : setOperation (tail li) la c
  | c == 'I' = setOperation (tail li) la c
  | c == 'S' && not (isNumOn (head li) la) =  head li : setOperation (tail li) la c
  | c == 'S' = setOperation (tail li) la c
  | otherwise = []

pairOf :: Char -> Char
pairOf bracket
  | bracket == '(' = ')'
  | bracket == '{' = '}'
  | otherwise = ']'

addedStack :: [Char] -> Char -> [Char]
addedStack lc c
  | null lc = [c]
  | not (null lc) && c == pairOf (last lc) = init lc
  | otherwise = lc ++ [c]

stackedResult :: [Char] -> [Char] -> [Char]
stackedResult stacked li
  | null li = stacked
  | otherwise = stackedResult (addedStack stacked (head li)) (tail li)

kurungBenar :: [Char] -> Bool
kurungBenar li = null (stackedResult "" li)



module AlternateSort where

minList :: [Int] -> Int
{- minList l mengembalikan nilai minimum dari seluruh elemen list -}
minList li
  | length li == 1 = head li
  | otherwise = min (head li) (minList (tail li))

maxList :: [Int] -> Int
maxList li
  | length li == 1 = head li
  | otherwise = max (head li) (maxList (tail li))

sortList :: [Int] -> ([Int]-> Int)-> [Int]
sortList li f
  | length li ==1 = [head li]
  | head li == f li = head li : sortList (tail li) f
  | otherwise = sortList (tail li ++ [head li]) f


generateList :: [Int] -> Int -> [Int]
generateList li n
  | n == 0 = []
  | otherwise = head li : generateList (tail li) (n-1)

reducedList :: [Int]-> Int -> [Int]
reducedList li n
  | n == 0 = li
  | otherwise = reducedList (tail li) (n-1)

generateI3 :: [Int]-> [Int]-> [Int]
generateI3 l1 l2
  | null l1 && null l2 = []
  | null l1 = l2
  | null l2 = l1
  | otherwise =  [head l1, head l2] ++ generateI3 (tail l1) (tail l2)

alternateSort :: [Int] -> [Int]
alternateSort li = l3 where
  sortedLi = sortList li minList
  halfLength = length li `div` 2 + length li `mod` 2
  halfLength2 = length li `div` 2
  l1 = sortList (generateList sortedLi halfLength) minList
  l2 = sortList (generateList (reducedList sortedLi halfLength) halfLength2) maxList
  l3 = generateI3 l1 l2

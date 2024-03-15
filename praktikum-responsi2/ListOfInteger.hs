module ListOfInteger where

isMember:: [Int]->Int ->  Bool
isMember li n = n `elem` li

minList :: [Int] -> Int
{- minList l mengembalikan nilai minimum dari seluruh elemen list -}
minList li
  | null li = 0
  | length li == 1 = head li
  | otherwise = min (head li) (minList (tail li))


nbX :: Int -> [Int] -> Int
{- nbX x l menghasilkan banyaknya kemunculan x pada l -}
nbX n li
  | null li = 0
  | n == head li = 1 + nbX n (tail li)
  | otherwise = nbX n (tail li)

maxList :: [Int] -> Int
{- maxList l mengembalikan nilai minimum dari seluruh elemen list -}
maxList li
  | null li = 0
  | length li == 1 = head li
  | otherwise = max (head li) (maxList (tail li))


jmlMin :: [Int] -> (Int,Int)
jmlMin li = (minNum, nbX minNum li) where
  minNum = minList li
{- jmlMin l menghasilkan tuple (a,b) dengan:
      a adalah nilai minimum dari elemen-elemen l dan

      b adalah jumlah kemunculan a pada l -}

maxNb :: [Int]-> (Int,Int) 
maxNb li = (maxNum, nbX maxNum li) where
  maxNum = maxList li

pecahListGanjilGenap:: [Int]-> ([Int],[Int],[Int])
pecahListGanjilGenap li = (listNega, listGanjil, listGenap) where
  listNega = filter (<= 0) li
  listGanjil = filter (\x -> odd x && x>0) li
  listGenap = filter (\x -> even x && x>0) li

isEqual :: [Int]-> [Int]-> Bool
isEqual l1 l2 
  | length l1 /= length l2 = False  
  | null l1 = True
  | head l1 == head l2 = isEqual (tail l2) (tail l2)
  | otherwise = False

pecahListPosNeg :: [Int]->([Int],[Int])
pecahListPosNeg li = (listPos,listNeg) where
  listNeg = filter (< 0) li
  listPos = filter (>=0) li
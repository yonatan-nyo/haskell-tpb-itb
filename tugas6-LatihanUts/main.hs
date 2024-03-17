-- Judul main(n)

-- Definisi dan Spesifikasi
ubahArah :: Int -> Int -> Int
  {- ubahArah mengembalikan arah pergerakan baru -}
-- Realisasi
ubahArah s r
  | s + r < 0 = (360+s+r)`mod` 360
  | otherwise = (s+r)`mod` 360
-- Aplikasi
-- ubahArah 50 100
-- ubahArah 350 100

-- Definisi dan Spesifikasi
pangkat :: Int -> Int -> Int
  {-Fungsi pangkat menerima masukan berupa dua buah integer, a dan b, dan mengembalikan hasil berupa a^b (a pangkat b). Masukan diasumsikan selalu valid, yaitu a > 0 dan b ≥ 0. Buatlah definisi, spesifikasi, realisasi dengan menggunakan  ekspresi rekursif  dan contoh aplikasi berikut hasilnya (minimum 2 buah) dari fungsi pangkat. -}
-- Realisasi
pangkat a b
  | b == 0 = 1
  | otherwise = a * pangkat a (b-1)
-- Aplikasi
-- pangkat 2 3
-- pangkat 2 1

isEmpty :: [a] -> Bool
isEmpty = null
-- Definisi dan Spesifikasi
isAllGanjil :: [Int]-> Bool
-- Realisasi
isAllGanjil li
  | isEmpty li = True
  | head li `mod` 2 == 1 = isAllGanjil (tail li)
  | otherwise = False

isOneElmt :: [Int] -> Bool
isOneElmt li = length li == 1
konso :: Int -> [Int] -> [Int]
konso a li = a : li
-- Definisi dan Spesifikasi
getSmallest :: [Int]-> Int
-- Realisasi
getSmallest li
  | isOneElmt li = head li
  | head li < head (tail li) = getSmallest (konso (head li) (tail (tail li)))
  | otherwise = getSmallest (tail li)

-- Definisi dan Spesifikasi
delElement :: Int -> [Int] -> [Int]
-- Realisasi
delElement a li
  | null li = []
  | a == head li = delElement a (tail li)
  | otherwise = konso (head li) (delElement a (tail li))

-- Definisi dan Spesifikasi
sortList :: [Int] -> [Int]
-- Realisasi
sortList li
  | length li == 1 = [head li]
  | head li < head right = head li : right
  | otherwise = head right : sortList (head li : tail right)
  where
    right = sortList(tail li)

-- Definisi dan Spesifikasi
offsetList :: (Float->Float)->(Float->Float)-> Float -> Float -> [Float]
{- mengembalikan range dari a ke b melalui fungsi g yang telah ditambahkan dengan f -}
-- Aplikasi
offsetList f g a b
  | a > b = []
  | otherwise = f a : offsetList f g (g a) b

-- Aplikasi
-- offsetList (\x -> x) (\x -> x + 1) (1.2) (7.1)
-- offsetList (\x -> if (x<0) then -999.0 else x+3.2) (\x -> x + 0.5) (-1.0) (1.0)
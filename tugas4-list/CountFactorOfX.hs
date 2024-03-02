-- Kemunculan Faktor Pada list countFactorOfX(n, l)
module CountFactorOfX where

-- Definisi dan Spesifikasi
countFactorOfX :: Int -> [Int] -> Int
  -- countFactorOfX n l mengembalikan banyaknya kemunculan ​bilangan yang merupakan faktor dari n pada l 
  -- n adalah Integer, l adalah list of Integer
getFactorCnt :: Int -> Int -> Int
  -- getFactorCnt mengembalikan Jumlah faktor pada angka tertentu
  -- n dan el adalah Integer

-- Realisasi
getFactorCnt n el 
  | el < n || el `mod` n /= 0 = 0 -- kalau sudah lebih kecil atau tidak habis dibagi, maka kemunculan faktor sudah tidak ada lagi
  | otherwise = 1 + getFactorCnt n (el `div` n) -- menambahkan faktor setiap angka dibagi

countFactorOfX n l
  | null l = 0 -- sudah tidak ada elemen
  | otherwise = getFactorCnt n (head l) + countFactorOfX  n (tail l) -- lanjut ke elemen selanjutnya
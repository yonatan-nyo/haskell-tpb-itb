module DeretSegitiga where

-- Deret Segitiga   deretSegitiga(n)

-- Definisi dan Spesifikasi
deretSegitiga :: Int -> Int
  -- deretSegitiga menentukan nilai bilangan ke-n pada deret segitiga.
  -- n adalah bilangan bulat
-- Realisasi
deretSegitiga n
  | n == 1    = 1 
  | otherwise = n + deretSegitiga (n - 1)


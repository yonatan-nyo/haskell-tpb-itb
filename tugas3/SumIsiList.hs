module SumIsiList where
-- Hasil Penjumlahan Elemen Integer sumIsiList(n)

-- Definisi dan Spesifikasi
sumIsiList :: [Int] -> Int
  -- sumIsiList menghitung hasil penjumlahan dari seluruh elemen sebuah list of integer l yang tidak kosong.
  -- l adalah list of integers

-- Realisasi
sumIsiList l 
  | length l == 1 = head l -- sisa satu kasih elemen
  | otherwise = head l + sumIsiList (tail l) -- ambil depan, rekursi belakang
  
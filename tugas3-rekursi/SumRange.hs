module SumRange where
-- Judul sumRange(a, b)

-- Definisi dan Spesifikasi
sumRange :: Int -> Int -> Int
  -- sumRange adalah fungsi yang menerima masukan 2 bilangan integer, misalnya a dan b yang menyatakan rentang bilangan dengan syarat: a <= b; a dan b bilangan positif; dan menghasilkan penjumlahan semua bilangan dari a s.d. b..
  -- a dan b adalah bilangan bulat
-- Realisasi
sumRange a b 
  | a==b = a
  | otherwise = a + sumRange (a+1) b
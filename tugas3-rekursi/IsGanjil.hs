module IsGanjil where
-- Judul isGanjil(n)

-- Definisi dan Spesifikasi
isGanjil  :: Int -> Bool
  -- isGanjil menentukan apakah sebuah bilangan integer (>= 0) merupakan bilangan ganjil..
  --  adalah
-- Realisasi
isGanjil n 
  | n <= 2 = n == 1
  | otherwise = isGanjil (n-2)
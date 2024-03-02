module LuasBS where
-- Judul luasBS(n)

-- Definisi dan Spesifikasi
luasBS :: Int -> Int
  -- luasBS menentukan .
  --  adalah
-- Realisasi
luasBS n 
  | n == 1 = 1
  | otherwise = n*2 - 1 + luasBS (n-1)
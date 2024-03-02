module DelNthElmt where
-- Judul delNthElmt(n)

-- Definisi dan Spesifikasi
delNthElmt :: Int -> [Char] -> [Char] 
  {- delNthElmt n l menghilangkan elemen ke-n dari l. Asumsi: n lebih kecil atau sama dengan jumlah elemen l; l tidak kosong -}
  -- n adalah Int L adalah List karakter

-- Realisasi
delNthElmt n l
  | null l = [] -- list sudah habis di sambung dengan kosong
  | n == 1 = delNthElmt (n-1) (tail l) -- skip jika ketemu index itu
  | length l > 1 = head l : delNthElmt (n-1) (tail l) -- ambil terus, geser nilai sampai ketemu
  | otherwise = [head l] -- sisa satu, masukin
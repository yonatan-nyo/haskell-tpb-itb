-- 19623271 (Yonatan Edward Njoto)
module FilterList where
-- Penyaringan terhadap elemen list filterList(isValid, list)

-- Definisi dan Spesifikasi
isPos:: Int-> Bool
  -- isPos mengecek apakah bilangan positif
isNeg:: Int-> Bool
  -- isPos mengecek apakah bilangan positif
isKabisat :: Int -> Bool
  -- isKabisat mengecek apakah tahun kabisat
filterList :: (Int-> Bool) -> [Int] -> [Int]
  -- filterList menyaring list berdasarkan kondisi yang diharapkan.
  -- isValid adalah fungsi yang menerima Integer dan mengembalikan boolean, list adalah list integer

-- Realisasi
isPos n = n >= 0 -- positif >= 0
isNeg n = n < 0 -- negatif < 0
isKabisat y = y `mod` 400 == 0 || (y `mod` 4 ==0 && y `mod` 100 /= 0) -- jika habis dibagi 400 atau (habis dibagi 4 dan tidak habis dibagi 100)

filterList isValid list
  | null list = [] -- jika sudah kosong balikkan saja
  | isValid (head list) = head list : filterList isValid (tail list) -- jika yang depan memenuhi kondisi dan belum kosong cek belakang juga
  | otherwise = filterList isValid (tail list) -- jika yang depan tidak memenuhi kondisi dan belum kosong cek belakang

-- Aplikasi

-- filterList isPos [0,-1,3]
-- [0,3]

-- filterList isNeg [0,-1,3]
-- [-1]

-- filterList isKabisat [400,200,2004,2003]
-- [400,2004]

-- filterList (\x -> x >= 0 && x `mod` 2 == 0) [400,200,2004,2003]
-- [400,200,2004]
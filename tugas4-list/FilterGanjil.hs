module FilterGanjil where
-- Memfilter nilai dalam array filterGanjil(n)

-- Definisi dan Spesifikasi
filterGanjil :: [Int] -> [Int]
  -- filterGanjil menentukan melakukan filtering terhadap sebuah list of integer li sehingga menghasilkan list dengan elemen yang hanya terdiri atas bilangan ganjil yang muncul di li dengan urutan kemunculan yang sama. .

-- Realisasi
filterGanjil l 
  | null l = []
  | head l `mod` 2 == 1 = head l : filterGanjil (tail l)
  | otherwise = filterGanjil (tail l)
module IsEqFront where
-- Judul isEqFront(n)

-- Definisi dan Spesifikasi
isEqFront :: [Char] -> [Char] -> Bool
  -- isEqFront yang menerima masukan 2 buah list of character yang tidak kosong, misalnya T1 dan T2 dan menghasilkan true jika potongan awal list T2 mengandung T1 (dengan panjang dan urutan karakter yang sama). Banyaknya elemen T2 selalu lebih dari atau sama dengan T1.​
  --  t1 dan t2 adalah array of characters

-- Realisasi
isEqFront t1 t2
  | length t1 /= length t2 = False -- different length = false
  | null t1 || null t2 = True -- stop checking if empty
  | head t1 /= head t2 = False -- different element (checking by head) = false
  | otherwise = isEqFront (tail t1) (tail t2) -- if length and head matches, proceed to check next element 
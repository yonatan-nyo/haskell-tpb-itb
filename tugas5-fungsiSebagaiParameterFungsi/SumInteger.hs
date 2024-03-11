-- 19623271 (Yonatan Edward Njoto)
module SumInteger where
-- Judul sumInteger(m,n,f)

-- Definisi dan Spesifikasi
sumInteger :: Int -> Int -> (Int -> Bool) -> Int
  -- sumInteger menentukan hasil penjumlahan antara m dan n yang memenuhi syarat dari fungsi f.
  -- m dan n adalah Integer, f adalah fungsi menerima Integer dan mengembalikan Boolean

-- Realisasi
sumInteger m n f 
  | m == n && f m = m -- saat sudah di posisi terakhir untuk di cek dan memenuhi syarat fungsi f, kembalikan angka itu
  | m == n = 0 -- jika sudah di posisi terakhir untuk di cek dan tidak memenuhi syarat fungsi f, kembalikan 0
  | f m = m + sumInteger (m+1) n f -- jika belum di posisi terkahit dan memenuhi fungsi f, hasil = angka itu ditambah pengecekan seterusnya
  | otherwise = sumInteger (m+1) n f -- jika belum di posisi terkahit dan tidak memenuhi fungsi f, hasil = pengecekan seterusnya

-- Aplikasi
-- sumInteger 1 100 (\x -> 100 `mod` x == 0)
-- 217

-- sumInteger 1 100 (\x -> (even x && x `mod` 10 == 0) || (odd x && x `mod` 5 == 0))
-- 1050

-- sumInteger 25 25 (\x -> x < 10)
-- 0
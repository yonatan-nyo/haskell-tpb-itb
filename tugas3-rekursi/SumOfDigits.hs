module SumOfDigits where
-- Judul sumOfDigits(n)

-- Definisi dan Spesifikasi
sumOfDigits :: Int -> Int
  -- sumOfDigits menentukan hasil penjumlahan dari setiap bilangan tunggal yang terdapat di dalam sebuah bilangan integer positif.
  -- n adalah bilangan bulat
-- Realisasi
sumOfDigits n 
  | n<=10 = n
  | otherwise = n `mod` 10 + sumOfDigits (n `div` 10)

-- Definisi dan Spesifikasi
sumOfDigitsPosNeg :: Int -> Int
  -- sumOfDigitsPosNeg menentukan hasil penjumlahan dari setiap bilangan tunggal yang terdapat di dalam sebuah bilangan integer positif dan negatif.
  -- n adalah bilangan bulat
-- Realisasi
sumOfDigitsPosNeg n = sumOfDigits (abs n)
-- JUDUL nilaiTengah(a, b, c)

-- DEFINISI DAN SPESIFIKASI
max3 :: Int -> Int -> Int -> Int
  -- max3 menentukan angka terbesar
  -- a, b, c (mewakilkan input) adalah bilangan bulat

-- REALISASI
max3 a b c
  | a >= b && a >= c = a
  | b >= a && b >= c = b
  | otherwise = c


-- DEFINISI DAN SPESIFIKASI
min3 :: Int -> Int -> Int -> Int
  -- min3 menentukan angka terkecil
  -- a, b, c (mewakilkan input) adalah bilangan bulat

-- REALISASI
min3 a b c
  | a <= b && a <= c = a
  | b <= a && b <= c = b
  | otherwise = c


-- DEFINISI DAN SPESIFIKASI
nilaiTengah :: Int -> Int -> Int -> Int
  -- nilaiTengah menentukan angka yang akan berada di tengah bila diurutkan
  -- a, b, c (mewakilkan input) adalah bilangan bulat

-- REALISASI
nilaiTengah a b c
  | a /= max3 a b c && a /= min3 a b c = a
  | b /= max3 a b c && b /= min3 a b c = b
  | otherwise = c


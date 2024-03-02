-- JUDUL isJamValid(j, m, d)

-- DEFINISI DAN SPESIFIKASI
isJamValid :: Int -> Int -> Int -> Bool
  -- isJamValid menentukan apakah jam, menit, dan detik yang kita berikan valid
  -- jam, menit, dan detik adalah bilangan bulat

-- REALISASI
isJamValid j m d = 0 <= j && j <= 23 && 0 <= m && m <= 59 && 0 <= d && d <= 59

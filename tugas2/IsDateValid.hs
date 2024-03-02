-- JUDUL isDateValid(d, m, y)

-- DEFINISI DAN SPESIFIKASI
isDateValid :: Int -> Int -> Int -> Bool
-- isDateValid menentukan apakah tanggal yang diberikan valid, dengan menggunakan tanggal hari, bulan, tahun
-- hari, bulan, tahun adalah bilangan bulat

-- REALISASI
isDateValid d m y
  | 1 <= m && m <= 12 && 0 <= y && y <= 99 = 
    if m == 2  -- February
      then if (y `mod` 4 == 0 && (y + 1900) `mod` 100 /= 0) || ((y + 1900) `mod` 400 == 0)
        then 1 <= d && d <= 29 -- Leap year
        else 1 <= d && d <= 28 -- Non-leap year

      else -- Not February 
        if m > 6
          then if m `mod` 2 == 1 -- Months 7, 9... have 31 days
            then 1 <= d && d <= 31
            else 1 <= d && d <= 30
          else if m `mod` 2 == 1 -- Months 1, 3, 5... have 31 days
            then 1 <= d && d <= 30
            else 1 <= d && d <= 31
  | otherwise = False

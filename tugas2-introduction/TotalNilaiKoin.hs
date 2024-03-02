-- JUDUL totalNilaiKoin(q, d, n, p)

-- DEFINISI DAN SPESIFIKASI
totalNilaiKoin :: Int -> Int -> Int -> Int -> (Int, Int)
  -- totalNilaiKoin menerima input sejumlah koin quarter, dime, nickel, dan penny
  -- dan menghasilkan pasangan nilai <dollar, sen> yang senilai dengan total koin-koin tersebut.
  -- q, d, n, p (mewakilkan input) adalah bilangan bulat

-- REALISASI
totalNilaiKoin q d n p = (dollars, cents)
  where
    totalCents = q * 25 + d * 10 + n * 5 + p
    dollars = totalCents `div` 100
    cents = totalCents `mod` 100

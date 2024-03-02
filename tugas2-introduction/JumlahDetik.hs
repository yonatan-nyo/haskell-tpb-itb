-- JUDUL jumlahDetik(j, m, d)

-- DEFINISI DAN SPESIFIKASI
jumlahDetik :: Int -> Int -> Int -> Int
  -- jumlahDetik menghitung jumlah detik dari jam tersebut terhitung mulai jam 0:0:0 pada tanggal ybs.
  -- jam, menit, dan detik adalah bilangan bulat

-- REALISASI
jumlahDetik j m d = (j*3600) + (m*60) + d

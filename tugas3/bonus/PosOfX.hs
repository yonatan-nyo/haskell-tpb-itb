module PosOfX where

posOfX :: Char -> [Char] -> Int
posOfX e lc
  | null lc = 0
  | e == head lc = 0
  | otherwise = 1 + posOfX e (tail lc)
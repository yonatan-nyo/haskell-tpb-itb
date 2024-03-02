module IsUnique where

contains :: Char -> [Char] -> Bool
contains _ [] = False
contains c lc
  | c == head lc    = True
  | otherwise = contains c (tail lc)

isUnique :: [Char] -> Bool
isUnique lc
  | null lc           = True
  | contains (head lc) (tail lc) = False
  | otherwise         = isUnique (tail lc)

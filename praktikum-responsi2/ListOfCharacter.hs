module ListOfCharacter where

konkat:: [Char]->[Char]-> [Char]
konkat l1 l2 = l1++l2

pajakMakan:: [Char]->[Int]->[Char]
pajakMakan lc li 
  | null li = ""
  | fromIntegral (head li) * 1.1 <= 200 = head lc : pajakMakan (tail lc) (tail li)
  | otherwise= pajakMakan (tail lc) (tail li)

isPalindrom:: [Char]-> Bool
isPalindrom lc
  | length lc <=1 = True
  | head lc == last lc = isPalindrom (tail (init lc))
  | otherwise = False

getOddEl :: [a] -> [a]
getOddEl []       = []        
getOddEl [x]      = [x]    
getOddEl (x:_:xs) = x : getOddEl xs

splitAlternate :: [a] -> ([a], [a])
splitAlternate lc 
  | length lc >= 2 = (getOddEl lc, getOddEl (tail lc))
  | length lc == 1 = (getOddEl lc, [])
  | otherwise = ([],[])
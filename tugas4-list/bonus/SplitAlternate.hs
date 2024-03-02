module SplitAlternate where

getOddEl :: [a] -> [a]
getOddEl []       = []        
getOddEl [x]      = [x]    
getOddEl (x:_:xs) = x : getOddEl xs

splitAlternate :: [a] -> ([a], [a])
splitAlternate lc = (getOddEl lc, getOddEl (tail lc))

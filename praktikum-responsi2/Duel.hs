module Duel where

duel:: [[Char]]-> [[Char]]
duel ls
  | null ls = []
  | head ls == "desperado" = "BANG" : duel (tail ls)
  | otherwise = head ls : duel (tail ls)
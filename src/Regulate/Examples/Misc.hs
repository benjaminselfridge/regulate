{-# LANGUAGE OverloadedStrings #-}

module Regulate.Examples.Misc where

import Regulate.NFA

csfs :: MNFA s Char
csfs = string "cs" `cat` star (string "fs")

sr :: MNFA s Char
sr = star ((symbol 's' `union` symbol 'r') `cat` symbol 'r')

scope :: Ord e => MNFA s e -> MNFA s e
scope = upto 4

actor1 :: NFA Char
actor1 = buildNFA $ scope (oneof "ab")

actor2 :: NFA Char
actor2 = buildNFA $ scope (oneof "c")

coord_ac :: NFA Char
coord_ac = buildNFA $ star "ac"

coord_bd :: NFA Char
coord_bd = buildNFA $ star "bd"

g :: NFA Char
g = actor1 `join` coord_ac `join` actor2

g' :: NFA Char
g' = coord_ac `join'` (actor1 `join` actor2)

cons :: Char -> (String, String) -> (String, String)
cons 'a' (ones, twos) = ('a':ones, twos)
cons 'b' (ones, twos) = ('b':ones, twos)
cons 'c' (ones, twos) = (ones, 'c':twos)
cons 'd' (ones, twos) = (ones, 'd':twos)
cons _ _ = error "cons"

big :: NFA String
big = buildNFA (scope $ oneof ["a1" , "b1" ]) `join'`
      buildNFA (scope $ oneof ["a2" , "b2" ]) `join'`
      buildNFA (scope $ oneof ["a3" , "b3" ]) `join'`
      buildNFA (scope $ oneof ["a4" , "b4" ])

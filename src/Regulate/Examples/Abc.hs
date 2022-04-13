{-# LANGUAGE OverloadedStrings #-}

module Regulate.Examples.Abc where

import Regulate.NFA
import Regulate.NFA.GraphViz

import qualified Data.Set as Set

data Event = A1 | B1 | A2 | B2
  deriving (Show, Read, Eq, Ord)

instance SymbolLabel Event where
  symbolLabel = eventLabel

scopeNum :: Int
scopeNum = 10

scope :: MNFA s Event -> MNFA s Event
scope = upto scopeNum

g :: NFA Event
g = buildNFA (scope (symbol A1 `union` symbol B1))
    `join'` buildNFA (scope (symbol A2 `union` symbol B2))

cons :: Event -> ([Event], [Event]) -> ([Event], [Event])
cons A1 (ones, twos) = (A1:ones, twos)
cons B1 (ones, twos) = (B1:ones, twos)
cons A2 (ones, twos) = (ones, A2:twos)
cons B2 (ones, twos) = (ones, B2:twos)

myNub :: Ord a => [a] -> [a]
myNub = Set.toList . Set.fromList

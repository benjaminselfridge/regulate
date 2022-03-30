module Regulate.NFA.Examples.Misc where

import Regulate.NFA

example1 :: NFA Int Char
example1 = buildNFA 0 (symbol 'a' `union` symbol 'b')

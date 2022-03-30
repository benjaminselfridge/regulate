module Regulate.NFA.Examples.Misc where

import Regulate.NFA

csfs :: MNFA s Char
csfs = symbol 'c' `cat` symbol 's' `cat` (star (symbol 'f' `cat` symbol 's'))

sr :: MNFA s Char
sr = star (symbol 's' `cat` symbol 'r')

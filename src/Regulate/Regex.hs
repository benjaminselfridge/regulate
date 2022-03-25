{-# LANGUAGE MonadComprehensions #-}

-- | Very simple module for generating strings from Kleene-style regular
-- expressions. Uses @control-monad-omega@ to get a breadth-first, rather than
-- depth-first, ordering of the string set. In this way, we can guarantee that
-- if @s@ is a string in the set of strings described by regular expression @r@,
-- then @elem s r@ will return @True@ in finite time.
--
-- Consider the expression @(0|(1(01*0)*1))*@, representing the strings
-- corresponding to all binary numbers that are multiples of three. We can
-- encode this as:
--
-- @
-- r = star (alt [ constant '0'
--               , cat [ constant '1'
--                     , star (cat [ constant '0'
--                           , (star (constant '1'))
--                           , constant '0'])
--                     , constant '1'
--                     ]
--               ])
-- @
--
-- Now, if we @generate r@, we get a infinite list of strings from the language
-- described by @r@:
--
-- >>> take 10 (generate r)
-- ["","0","11","00","1001","011","000","10101","110","0011"]
--
-- Eventually, every string in the language will show up (but it might take a
-- while!)
--
-- Another useful function for generating regular expressions is `scope`, which
-- can replace every @+@ operator in a regular expression with an "unrolling" of
-- said operator a finite number of times:
--
-- >>> r = plus (alt [constant 'a', constant 'b'])
-- >>> putStrLn $ ppRegex r
-- (+ ( 'a' | 'b' ) +)
-- >>> r' = scope 2 r
-- >>> putStrLn $ ppRegex r'
-- ( ( 'a' | 'b' ) ( ε | ( 'a' | 'b' ) ) )
-- >>> generate r'
-- ["a","aa","b","ab","ba","bb"]
module Regulate.Regex
  ( Regex(..)
  , ppRegex
  , generate
  -- * Building regular expressions
  , emptySet
  , emptyString
  , constant
  , oneOf
  , optional
  , alt
  , cat
  , plus
  , star
  , pow
  , upTo
  , upTo1
  , scope
  ) where

import Control.Monad.Omega

-- | Kleene-style regular expressions.
data Regex a
  = EmptySet
  | EmptyString
  | Constant a
  | Alt (Regex a) (Regex a)
  | Cat (Regex a) (Regex a)
  | Plus (Regex a)
  deriving (Show, Read)

-- | Pretty-print a regular expression.
ppRegex :: Show a => Regex a -> String
ppRegex EmptySet = "∅"
ppRegex EmptyString = "ε"
ppRegex (Constant a) = show a
ppRegex (Alt r s) = "( " ++ ppRegex r ++ " | " ++ ppRegex s ++ " )"
ppRegex (Cat r s) = "( " ++ ppRegex r ++ " " ++ ppRegex s ++ " )"
ppRegex (Plus r) = "(+ " ++ ppRegex r ++ " +)"

-- | The empty set of strings.
emptySet :: Regex a
emptySet = EmptySet

-- | The singleton set containing the empty string.
emptyString :: Regex a
emptyString = EmptyString

-- | The singleton set containing the string of length one corresponding to the
-- given token.
constant :: a -> Regex a
constant = Constant

-- | The set of all strings of length one from a given token set.
oneOf :: [a] -> Regex a
oneOf = alt . map Constant

-- | Given a regular expression, add the empty string to its set.
optional :: Regex a -> Regex a
optional = Alt EmptyString

-- | @alt [a, b, c, ..] === (a|b|c|...)@
alt :: [Regex a] -> Regex a
alt [] = EmptySet
alt as = foldr1 Alt as

-- | @cat [a, b, c, ..] === abc...@
cat :: [Regex a] -> Regex a
cat [] = EmptyString
cat as = foldr1 Cat as

-- | The @+@ operator (one or more concatenated).
plus :: Regex a -> Regex a
plus = Plus

-- | The @*@ operator (zero or more concatenated).
star :: Regex a -> Regex a
star = Alt EmptyString . Plus

-- | The @^@ operator (exactly n).
pow :: Regex a -> Int -> Regex a
pow r i | i <= 0 = EmptyString
        | otherwise = foldr1 Cat (replicate i r)

-- | Like @*@, but limits the number of repeats to a certain depth. If given a
-- negative integer, silently treats it as @0@.
upTo :: Regex a -> Int -> Regex a
upTo r i | i <= 0 = EmptyString
         | otherwise = optional (Cat r (r `upTo` (i-1)))

-- | Like @+@, but limits the number of repeats to a certain depth. If given a
-- non-positive integer, silently treats it as @1@.
upTo1 :: Regex a -> Int -> Regex a
upTo1 r i | i <= 1 = r
          | otherwise = (Cat r (optional (r `upTo1` (i-1))))

-- | Generate all strings from a given regular expression in a sensible order
-- (breadth-first).
generate :: Regex a -> [[a]]
generate EmptySet = []
generate EmptyString = [[]]
generate (Constant a) = [[a]]
generate (Alt r s) = diagonal [generate r, generate s]
generate (Cat r s) = runOmega [ x ++ y | x <- each (generate r)
                                       , y <- each (generate s) ]
generate (Plus r) = diagonal (generate . cat <$> tau r)

-- | "Scope" a regular expression by replacing all the plus operators with
-- `upto1` operators.
scope :: Int
         -- ^ how many times to repeat each + operator (must be >0)
      -> Regex a
      -> Regex a
scope i (Alt r s) = Alt (scope i r) (scope i s)
scope i (Cat r s) = Cat (scope i r) (scope i s)
scope i (Plus r) = upTo1 (scope i r) i
scope _ r = r

-- | Given a single a, produce the infinite list @[[a], [a,a], [a,a,a], ...]
tau :: a -> [[a]]
tau a = [a] : [ a : as | as <- tau a ]

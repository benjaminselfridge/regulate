{-# LANGUAGE MonadComprehensions #-}

-- Very simple module for generating strings from Kleene-style regular
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

-- | Implementation of Kleene-style regular expressions.
module Regulate.Regex
  ( Regex(..)
  , ppRegex
  -- * Building regular expressions
  , empty
  , epsilon
  , symbol
  , string
  , union, (>|<)
  , cat, (><)
  , optional
  , oneof
  , pow
  , upto
  , star
  ) where

-- | Kleene-style regular expressions.
data Regex a
  = Empty
  | Epsilon
  | Symbol a
  | Union (Regex a) (Regex a)
  | Cat (Regex a) (Regex a)
  | Star (Regex a)
  deriving (Show, Read)

-- | Pretty-print a regular expression.
ppRegex :: Show a => Regex a -> String
ppRegex Empty = "∅"
ppRegex Epsilon = "ε"
ppRegex (Symbol a) = show a
ppRegex (Union r s) = "(" ++ ppRegex r ++ "|" ++ ppRegex s ++ ")"
ppRegex (Cat r s) = "(" ++ ppRegex r ++ ppRegex s ++ ")"
ppRegex (Star r) = "(" ++ ppRegex r ++ ")*"

-- | The empty set of strings.
empty :: Regex a
empty = Empty

-- | The singleton set containing the empty string.
epsilon :: Regex a
epsilon = Epsilon

-- | The singleton set containing the string of length one corresponding to the
-- given token.
symbol :: a -> Regex a
symbol = Symbol

-- | Create a regex for a particular string.
string :: [a] -> Regex a
string [] = epsilon
string (a:as) = symbol a >< string as

-- | Infix 'union'.
(>|<) :: Regex a -> Regex a -> Regex a
(>|<) = union
infixl 6 >|<

-- | Take the union of two regexes.
union :: Regex a -> Regex a -> Regex a
union = Union

-- | Infix 'cat'.
(><) :: Regex a -> Regex a -> Regex a
(><) = cat
infixl 7 ><

-- | Concatenate two regexes.
cat :: Regex a -> Regex a -> Regex a
cat = Cat

-- | Zero or one. Equivalent to 'union'ing with 'epsilon'.
optional :: Regex a -> Regex a
optional = (epsilon >|<)

-- | Union of symbols.
oneof :: [a] -> Regex a
oneof as = foldr1 union (symbol <$> as)

-- | Repeat a regex some number of times.
pow :: Int -> Regex a -> Regex a
pow i _ | i <= 0 = epsilon
pow i e = e >< pow (i-1) e

-- | Like 'pow', but accepts w^0, w^1, ..., w^n.
upto :: Int -> Regex a -> Regex a
upto i _ | i <= 0 = epsilon
upto i e = epsilon >|< (e >< upto (i-1) e)

-- | Kleene-* of two regexes.
star :: Regex a -> Regex a
star = Star

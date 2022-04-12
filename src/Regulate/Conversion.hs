{-# LANGUAGE RankNTypes #-}

-- | Module for converting between various representations of regular languages.
module Regulate.Conversion
  ( regexToMNFA
  , regexToNFA
  ) where

import           Regulate.NFA (NFA, MNFA, buildNFA)
import qualified Regulate.NFA as NFA
import           Regulate.Regex ( Regex(..) )

-- | Convert a regex to an 'MNFA'.
regexToMNFA :: Ord a => Regex a -> MNFA s a
regexToMNFA Empty = NFA.empty
regexToMNFA Epsilon = NFA.epsilon
regexToMNFA (Symbol a) = NFA.symbol a
regexToMNFA (Union r s) = NFA.union (regexToMNFA r) (regexToMNFA s)
regexToMNFA (Cat r s) = NFA.cat (regexToMNFA r) (regexToMNFA s)
regexToMNFA (Star r) = NFA.star (regexToMNFA r)

-- | Convert a regex to an 'NFA'.
regexToNFA :: Ord a => Regex a -> NFA a
regexToNFA r = buildNFA (regexToMNFA r)

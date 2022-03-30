module Regulate.NFA.Examples.DataFlow where

import Regulate.NFA.Haggle
import Regulate.NFA.Examples.Helpers

import Control.Monad.ST
import Data.Graph.Haggle
import qualified Data.Set as Set

data Event = Writer_Working
           | Writer_Writing
           | Reader_Reading
           | Reader_Working
  deriving (Show, Read, Eq, Ord)

writer :: NFA Int Event
writer = runST $ do
  g <- newNFAGraph
  ig <- newIntGen

  q0 <- addLabeledVertex' g ig

  addLabeledEdge' g q0 q0 Writer_Working
  addLabeledEdge' g q0 q0 Writer_Writing

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

reader :: NFA Int Event
reader = runST $ do
  g <- newNFAGraph
  ig <- newIntGen

  q0 <- addLabeledVertex' g ig

  addLabeledEdge' g q0 q0 Reader_Reading
  addLabeledEdge' g q0 q0 Reader_Working

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

writer_reader :: NFA [Int] Event
writer_reader = f writer `join`
                f (coordinate Writer_Writing Reader_Reading) `join`
                f reader
  where f = nfaMap (:[]) id

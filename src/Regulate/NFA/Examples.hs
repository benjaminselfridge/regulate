module Regulate.NFA.Examples where

import Regulate.NFA

import Control.Monad (void, forM_)
import Control.Monad.ST
import Data.Graph.Haggle
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | NFA for cs(fs)*
example1 :: NFA Char
example1 = runST $ do
  g <- newEdgeLabeledGraph newMDigraph

  q0 <- addVertex g
  q1 <- addVertex g
  q2 <- addVertex g
  q3 <- addVertex g

  void $ addLabeledEdge g q0 q1 'c'
  void $ addLabeledEdge g q1 q2 's'
  void $ addLabeledEdge g q2 q3 'f'
  void $ addLabeledEdge g q3 q2 's'

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q2)

-- | NFA for (sr)*
example2 :: NFA Char
example2 = runST $ do
  g <- newEdgeLabeledGraph newMDigraph

  q0 <- addVertex g
  q1 <- addVertex g

  void $ addLabeledEdge g q0 q1 's'
  void $ addLabeledEdge g q1 q0 'r'

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

example1_2 :: NFA Char
example1_2 = join example1 example2

-- | NFA for (a|b|c|d|e|f|g|h)^3.
example3 :: NFA Char
example3 = runST $ do
  g <- newEdgeLabeledGraph newMDigraph

  q0 <- addVertex g
  q1 <- addVertex g
  q2 <- addVertex g
  q3 <- addVertex g

  forM_ ['a'..'h'] $ \c -> do
    void $ addLabeledEdge g q0 q1 c
    void $ addLabeledEdge g q1 q2 c
    void $ addLabeledEdge g q2 q3 c

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q3)

example3_3 :: NFA Char
example3_3 = join example3 example3

example4 :: NFA Char
example4 = runST $ do
  g <- newEdgeLabeledGraph newMDigraph

  q0 <- addVertex g
  q1 <- addVertex g
  q2 <- addVertex g
  q3 <- addVertex g

  forM_ ['b'..'i'] $ \c -> do
    void $ addLabeledEdge g q0 q1 c
    void $ addLabeledEdge g q1 q2 c
    void $ addLabeledEdge g q2 q3 c

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q3)

example3_4 :: NFA Char
example3_4 = join example3 example4

data AliceBobEvent = CreatePlan
           | SendPlan
           | RevisePlan
           | ReceivePlan
           | CreateImpl
           | CheckImpl
           | ReviseImpl
           | RejectPlan
  deriving (Show, Read, Eq, Ord)

alice :: NFA AliceBobEvent
alice = runST $ do
  g <- newEdgeLabeledGraph newMDigraph

  q0 <- addVertex g
  q1 <- addVertex g
  q2 <- addVertex g
  q3 <- addVertex g

  void $ addLabeledEdge g q0 q1 CreatePlan
  void $ addLabeledEdge g q1 q2 SendPlan
  void $ addLabeledEdge g q2 q3 RevisePlan
  void $ addLabeledEdge g q3 q2 SendPlan

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q2)

bob :: NFA AliceBobEvent
bob = runST $ do
  g <- newEdgeLabeledGraph newMDigraph

  q0 <- addVertex g
  q1 <- addVertex g
  q2 <- addVertex g
  q3 <- addVertex g
  q4 <- addVertex g
  q5 <- addVertex g
  q6 <- addVertex g

  void $ addLabeledEdge g q0 q1 ReceivePlan
  void $ addLabeledEdge g q1 q2 CreateImpl
  void $ addLabeledEdge g q2 q3 CheckImpl
  void $ addLabeledEdge g q3 q4 ReviseImpl
  void $ addLabeledEdge g q4 q3 CheckImpl
  void $ addLabeledEdge g q3 q5 RejectPlan
  void $ addLabeledEdge g q5 q6 ReceivePlan
  void $ addLabeledEdge g q6 q3 CheckImpl

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q3)

coordinateSendReceive :: NFA AliceBobEvent
coordinateSendReceive = runST $ do
  g <- newEdgeLabeledGraph newMDigraph

  q0 <- addVertex g
  q1 <- addVertex g

  void $ addLabeledEdge g q0 q1 SendPlan
  void $ addLabeledEdge g q1 q0 ReceivePlan

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

coordinateRejectRevise :: NFA AliceBobEvent
coordinateRejectRevise = runST $ do
  g <- newEdgeLabeledGraph newMDigraph

  q0 <- addVertex g
  q1 <- addVertex g

  void $ addLabeledEdge g q0 q1 RejectPlan
  void $ addLabeledEdge g q1 q0 RevisePlan

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

alice_bob_coordinated :: NFA AliceBobEvent
alice_bob_coordinated =
  alice `join` bob `join` coordinateSendReceive `join` coordinateRejectRevise

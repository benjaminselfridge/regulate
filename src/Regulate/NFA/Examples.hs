{-# LANGUAGE GADTs #-}

module Regulate.NFA.Examples where

import Regulate.NFA.Examples.Helpers
import Regulate.NFA.Haggle

import Control.Monad (void, forM_, replicateM)
import Control.Monad.ST
import Data.Graph.Haggle
import qualified Data.Set as Set

----------------------------------------
-- EXAMPLES
----------------------------------------

-- | NFA for cs(fs)*
example1 :: NFA Int Char
example1 = runST $ do
  g <- newNFAGraph
  ig <- newIntGen

  [q0, q1, q2, q3] <- replicateM 4 (addLabeledVertex' g ig)

  addLabeledEdge' g q0 q1 'c'
  addLabeledEdge' g q1 q2 's'
  addLabeledEdge' g q2 q3 'f'
  addLabeledEdge' g q3 q2 's'

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q2)

-- | NFA for (sr)*
example2 :: NFA Int Char
example2 = runST $ do
  g <- newNFAGraph
  ig <- newIntGen

  [q0, q1] <- replicateM 4 (addLabeledVertex' g ig)

  addLabeledEdge' g q0 q1 's'
  addLabeledEdge' g q1 q0 'r'

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

example1_2 :: NFA [Int] Char
example1_2 = listLabels example1 `join` listLabels example2

-- | NFA for (a|b|c|d|e|f|g|h)^3.
example3 :: NFA Int Char
example3 = runST $ do
  g <- newLabeledGraph newMBiDigraph

  q0 <- addLabeledVertex g 0
  q1 <- addLabeledVertex g 1
  q2 <- addLabeledVertex g 2
  q3 <- addLabeledVertex g 3

  forM_ ['a'..'h'] $ \c -> do
    void $ addLabeledEdge g q0 q1 c
    void $ addLabeledEdge g q1 q2 c
    void $ addLabeledEdge g q2 q3 c

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q3)

example3_3 :: NFA [Int] Char
example3_3 = listLabels example3 `join` listLabels example3

example4 :: NFA Int Char
example4 = runST $ do
  g <- newLabeledGraph newMBiDigraph

  q0 <- addLabeledVertex g 0
  q1 <- addLabeledVertex g 1
  q2 <- addLabeledVertex g 2
  q3 <- addLabeledVertex g 3

  forM_ ['b'..'i'] $ \c -> do
    void $ addLabeledEdge g q0 q1 c
    void $ addLabeledEdge g q1 q2 c
    void $ addLabeledEdge g q2 q3 c

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q3)

example3_4 :: NFA [Int] Char
example3_4 = listLabels example3 `join` listLabels example4

data AliceBobEvent = Create_Plan
                   | Send_Plan
                   | Revise_Plan
                   | Receive_Plan
                   | Create_Impl
                   | Check_Impl
                   | Revise_Impl
                   | Reject_Plan
  deriving (Show, Read, Eq, Ord)

alice :: NFA Int AliceBobEvent
alice = runST $ do
  g <- newLabeledGraph newMBiDigraph

  q0 <- addLabeledVertex g 0
  q1 <- addLabeledVertex g 1
  q2 <- addLabeledVertex g 2
  q3 <- addLabeledVertex g 3

  void $ addLabeledEdge g q0 q1 Create_Plan
  void $ addLabeledEdge g q1 q2 Send_Plan
  void $ addLabeledEdge g q2 q3 Revise_Plan
  void $ addLabeledEdge g q3 q2 Send_Plan

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q2)

bob :: NFA Int AliceBobEvent
bob = runST $ do
  g <- newLabeledGraph newMBiDigraph

  q0 <- addLabeledVertex g 0
  q1 <- addLabeledVertex g 1
  q2 <- addLabeledVertex g 2
  q3 <- addLabeledVertex g 3
  q4 <- addLabeledVertex g 4
  q5 <- addLabeledVertex g 5
  q6 <- addLabeledVertex g 6

  void $ addLabeledEdge g q0 q1 Receive_Plan
  void $ addLabeledEdge g q1 q2 Create_Impl
  void $ addLabeledEdge g q2 q3 Check_Impl
  void $ addLabeledEdge g q3 q4 Revise_Impl
  void $ addLabeledEdge g q4 q3 Check_Impl
  void $ addLabeledEdge g q3 q5 Reject_Plan
  void $ addLabeledEdge g q5 q6 Receive_Plan
  void $ addLabeledEdge g q6 q3 Check_Impl

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q3)

alice_bob :: NFA [Int] AliceBobEvent
alice_bob =
  listLabels alice `join`
  listLabels (coordinate Send_Plan Receive_Plan) `join`
  listLabels (coordinate Reject_Plan Revise_Plan) `join`
  listLabels bob

----------------------------------------
-- ATM withdrawal


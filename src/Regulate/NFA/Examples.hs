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

  NFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q2)

-- | NFA for (sr)*
example2 :: NFA Char
example2 = runST $ do
  g <- newEdgeLabeledGraph newMDigraph

  q0 <- addVertex g
  q1 <- addVertex g

  void $ addLabeledEdge g q0 q1 's'
  void $ addLabeledEdge g q1 q0 'r'

  NFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

example1_2 :: NFA (Either3 Char Char Char)
example1_2 =
  let example1' =
        nfaMap (\c -> if c == 's' then Right c else Left c) example1
      example2' =
        nfaMap (\c -> if c == 's' then Left c else Right c) example2

  in join example1' example2'

vizJoined :: NFA (Either3 String String String) -> NFA String
vizJoined = nfaMap showEither3
  where showEither3 (Left3 a) = a
        showEither3 (Middle3 a) = a
        showEither3 (Right3 a) = a

either3Map :: (a -> b) -> Either3 a a a -> Either3 b b b
either3Map f (Left3 a) = Left3 (f a)
either3Map f (Middle3 a) = Middle3 (f a)
either3Map f (Right3 a) = Right3 (f a)

genJoined :: NFA (Either3 Char Char Char) -> NFA (Seq Char, Seq Char)
genJoined = nfaMap joinSeq
  where
    joinSeq (Left3 a) = (Seq.singleton a, Seq.empty)
    joinSeq (Middle3 a) = (Seq.singleton a, Seq.singleton a)
    joinSeq (Right3 a) = (Seq.empty, Seq.singleton a)

-- | For visualizing.
example1_2Viz :: NFA String
example1_2Viz = vizJoined (nfaMap (either3Map (:[])) example1_2)

-- | For generating pairs.
example1_2Gen :: NFA (Seq Char, Seq Char)
example1_2Gen = genJoined example1_2

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

  NFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q3)

example3Gen :: NFA (Seq Char)
example3Gen = nfaMap Seq.singleton example3

example3Viz :: NFA String
example3Viz = nfaMap (:[]) example3

example3_3 :: NFA (Either3 Char Char Char)
example3_3 = join (nfaMap Right example3) (nfaMap Left example3)

example3_3Viz :: NFA String
example3_3Viz = vizJoined (nfaMap (either3Map (:[])) example3_3)

example3_3Gen :: NFA (Seq Char, Seq Char)
example3_3Gen = genJoined example3_3

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

  NFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q3)

example3_4 :: NFA (Either3 Char Char Char)
example3_4 = join (nfaMap f example3) (nfaMap g example4)
  where f 'a' = Left 'a'
        f c = Right c

        g 'i' = Right 'i'
        g c = Left c

example3_4Viz :: NFA String
example3_4Viz = vizJoined (nfaMap (either3Map (:[])) example3_4)

example3_4Gen :: NFA (Seq Char, Seq Char)
example3_4Gen = genJoined example3_4

data Event = CreatePlan
           | SendPlan
           | RevisePlan
           | ReceivePlan
           | CreateImpl
           | CheckImpl
           | ReviseImpl
           | RejectPlan
  deriving (Show, Read, Eq, Ord)

alice :: NFA Event
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

  NFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q2)

bob :: NFA Event
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

  NFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q3)

coordinateSendReceive :: NFA Event
coordinateSendReceive = runST $ do
  g <- newEdgeLabeledGraph newMDigraph

  q0 <- addVertex g
  q1 <- addVertex g

  void $ addLabeledEdge g q0 q1 SendPlan
  void $ addLabeledEdge g q1 q0 ReceivePlan

  NFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

coordinateRejectRevise :: NFA Event
coordinateRejectRevise = runST $ do
  g <- newEdgeLabeledGraph newMDigraph

  q0 <- addVertex g
  q1 <- addVertex g

  void $ addLabeledEdge g q0 q1 RejectPlan
  void $ addLabeledEdge g q1 q0 RevisePlan

  NFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

alice_coordinateSendReceive :: NFA (Either3 Event Event Event)
alice_coordinateSendReceive =
  join (nfaMap f alice) (nfaMap g coordinateSendReceive)
  where f SendPlan = Right SendPlan
        f e = Left e

        g SendPlan = Left SendPlan
        g e = Right e

alice_coordinateSendReceiveViz :: NFA String
alice_coordinateSendReceiveViz =
  vizJoined (nfaMap (either3Map show) alice_coordinateSendReceive)

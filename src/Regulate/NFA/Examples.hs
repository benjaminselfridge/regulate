{-# LANGUAGE GADTs #-}

module Regulate.NFA.Examples where

import Regulate.NFA.Haggle

import Control.Monad (void, forM_, replicateM_, replicateM)
import Control.Monad.ST
import Data.Char
import Data.Char.Small
import Data.Graph.Haggle
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.STRef

-- | Helper functions for generating new vertices
type IntGen s = STRef s Int

newIntGen :: ST s (IntGen s)
newIntGen = newSTRef 0

getNextInt :: IntGen s -> ST s Int
getNextInt iRef = do
  i <- readSTRef iRef
  modifySTRef iRef (+1)
  return i

newVertex :: (MLabeledVertex g, MVertexLabel g ~ Int)
          => g (ST s) -> IntGen s -> ST s Vertex
newVertex g ig = addLabeledVertex g =<< getNextInt ig

-- | NFA for cs(fs)*
example1 :: NFA Int Char
example1 = runST $ do
  g <- newLabeledGraph newMBiDigraph

  q0 <- addLabeledVertex g 0
  q1 <- addLabeledVertex g 1
  q2 <- addLabeledVertex g 2
  q3 <- addLabeledVertex g 3

  void $ addLabeledEdge g q0 q1 'c'
  void $ addLabeledEdge g q1 q2 's'
  void $ addLabeledEdge g q2 q3 'f'
  void $ addLabeledEdge g q3 q2 's'

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q2)

-- | NFA for (sr)*
example2 :: NFA Int Char
example2 = runST $ do
  g <- newLabeledGraph newMBiDigraph

  q0 <- addLabeledVertex g 0
  q1 <- addLabeledVertex g 1

  void $ addLabeledEdge g q0 q1 's'
  void $ addLabeledEdge g q1 q0 'r'

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

listLabels :: Ord sigma => NFA nl sigma -> NFA [nl] sigma
listLabels = nfaMap (:[]) id

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

data AliceBobEvent = CreatePlan
                   | SendPlan
                   | RevisePlan
                   | ReceivePlan
                   | CreateImpl
                   | CheckImpl
                   | ReviseImpl
                   | RejectPlan
  deriving (Show, Read, Eq, Ord)

alice :: NFA Int AliceBobEvent
alice = runST $ do
  g <- newLabeledGraph newMBiDigraph

  q0 <- addLabeledVertex g 0
  q1 <- addLabeledVertex g 1
  q2 <- addLabeledVertex g 2
  q3 <- addLabeledVertex g 3

  void $ addLabeledEdge g q0 q1 CreatePlan
  void $ addLabeledEdge g q1 q2 SendPlan
  void $ addLabeledEdge g q2 q3 RevisePlan
  void $ addLabeledEdge g q3 q2 SendPlan

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

  void $ addLabeledEdge g q0 q1 ReceivePlan
  void $ addLabeledEdge g q1 q2 CreateImpl
  void $ addLabeledEdge g q2 q3 CheckImpl
  void $ addLabeledEdge g q3 q4 ReviseImpl
  void $ addLabeledEdge g q4 q3 CheckImpl
  void $ addLabeledEdge g q3 q5 RejectPlan
  void $ addLabeledEdge g q5 q6 ReceivePlan
  void $ addLabeledEdge g q6 q3 CheckImpl

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q3)

coordinateSendReceive :: NFA Int AliceBobEvent
coordinateSendReceive = runST $ do
  g <- newLabeledGraph newMBiDigraph

  q0 <- addLabeledVertex g 0
  q1 <- addLabeledVertex g 1

  void $ addLabeledEdge g q0 q1 SendPlan
  void $ addLabeledEdge g q1 q0 ReceivePlan

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

coordinateRejectRevise :: NFA Int AliceBobEvent
coordinateRejectRevise = runST $ do
  g <- newLabeledGraph newMBiDigraph

  q0 <- addLabeledVertex g 0
  q1 <- addLabeledVertex g 1

  void $ addLabeledEdge g q0 q1 RejectPlan
  void $ addLabeledEdge g q1 q0 RevisePlan

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

alice_bob_coordinated :: NFA [Int] AliceBobEvent
alice_bob_coordinated =
  listLabels alice `join`
  listLabels bob `join`
  listLabels coordinateSendReceive `join`
  listLabels coordinateRejectRevise

showStateLabel :: [Int] -> String
showStateLabel [i] = ['q' , (fromJust . toSub . intToDigit) i]
showStateLabel is = show' $ ('q':) . (:[]) . fromJust . toSub . intToDigit <$> is
  where show' s = '(' : (intercalate "," s) ++ ")"

----------------------------------------
-- ATM withdrawal

data ATMEvent
  -- User events
  = Insert_Card
  | Identification_Succeeds
  | Request_Withdrawal
  | Get_Money
  | Insufficient_Funds
  | Identification_Fails
  | Retrieve_Card
  -- ATM events
  | Idle
  | Read_Card
  | Validate_Id
  | Id_Successful
  | Report_Id_Failure
  | Check_Balance
  | Sufficient_Balance
  | Report_Insufficient_Balance
  | Dispense_Money
  deriving (Show, Read, Eq, Ord)

ppEvent :: Show a => a -> String
ppEvent = map (toLower . space) . show
  where space '_' = ' '
        space c = c

customer :: NFA Int ATMEvent
customer = runST $ do
  g <- newLabeledGraph newMBiDigraph

  q0 <- addLabeledVertex g 0
  q1 <- addLabeledVertex g 1
  q2 <- addLabeledVertex g 2
  q3 <- addLabeledVertex g 3
  q4 <- addLabeledVertex g 4

  void $ addLabeledEdge g q0 q1 Insert_Card
  void $ addLabeledEdge g q1 q2 Identification_Succeeds
  void $ addLabeledEdge g q1 q4 Identification_Fails
  void $ addLabeledEdge g q2 q3 Request_Withdrawal
  void $ addLabeledEdge g q3 q4 Get_Money
  void $ addLabeledEdge g q3 q4 Insufficient_Funds
  void $ addLabeledEdge g q4 q0 Retrieve_Card

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

customer_scoped :: Int -> NFA Int ATMEvent
customer_scoped scope = runST $ do
  g <- newLabeledGraph newMBiDigraph
  ig <- newIntGen

  q0 <- newVertex g ig

  startRef <- newSTRef q0
  finalStatesRef <- newSTRef (Set.singleton q0)

  replicateM_ scope $ do
    start <- readSTRef startRef
    q1 <- newVertex g ig
    q2 <- newVertex g ig
    q3 <- newVertex g ig
    q4 <- newVertex g ig
    q5 <- newVertex g ig

    void $ addLabeledEdge g start q1 Insert_Card
    void $ addLabeledEdge g q1 q2 Identification_Succeeds
    void $ addLabeledEdge g q1 q4 Identification_Fails
    void $ addLabeledEdge g q2 q3 Request_Withdrawal
    void $ addLabeledEdge g q3 q4 Get_Money
    void $ addLabeledEdge g q3 q4 Insufficient_Funds
    void $ addLabeledEdge g q4 q5 Retrieve_Card

    modifySTRef finalStatesRef (Set.insert q5)
    writeSTRef startRef q5

  mkNFA <$> freeze g <*> pure q0 <*> readSTRef finalStatesRef

atm :: NFA Int ATMEvent
atm = runST $ do
  g <- newLabeledGraph newMBiDigraph
  ig <- newIntGen

  [q0, q1, q2, q3, q4, q5, q6, q7, q8] <- replicateM 9 (newVertex g ig)

  void $ addLabeledEdge g q0 q1 Idle
  void $ addLabeledEdge g q1 q2 Read_Card
  void $ addLabeledEdge g q2 q3 Validate_Id
  void $ addLabeledEdge g q3 q4 Id_Successful
  void $ addLabeledEdge g q3 q8 Report_Id_Failure
  void $ addLabeledEdge g q4 q5 Check_Balance
  void $ addLabeledEdge g q5 q6 Sufficient_Balance
  void $ addLabeledEdge g q5 q8 Report_Insufficient_Balance
  void $ addLabeledEdge g q6 q7 Dispense_Money
  void $ addLabeledEdge g q7 q1 Idle
  void $ addLabeledEdge g q8 q1 Idle

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q1)

customer_atm :: NFA [Int] ATMEvent
customer_atm =
  listLabels customer `join`
  listLabels (coordinate Insert_Card Read_Card) `join`
  listLabels atm

customer_atm_scoped :: Int -> NFA [Int] ATMEvent
customer_atm_scoped scope =
  listLabels (customer_scoped scope) `join`
  listLabels (coordinate Insert_Card Read_Card) `join`
  listLabels atm

coordinate :: Ord s => s -> s -> NFA Int s
coordinate s1 s2 = runST $ do
  g <- newLabeledGraph newMBiDigraph
  ig <- newIntGen

  [q0, q1] <- replicateM 2 (newVertex g ig)

  void $ addLabeledEdge g q0 q1 s1
  void $ addLabeledEdge g q1 q0 s2

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

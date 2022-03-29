{-# LANGUAGE GADTs #-}

module Regulate.NFA.Examples where

import Regulate.NFA.Haggle

import Control.Monad (void, forM_, replicateM_, replicateM)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef)
import Control.Monad.ST
import Data.Char
import Data.Char.Small
import Data.Graph.Haggle
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.STRef

----------------------------------------
-- HELPER FUNCTIONS
----------------------------------------

type IntGen s = STRef s Int

newIntGen :: ST s (IntGen s)
newIntGen = newSTRef 0

getNextInt :: IntGen s -> ST s Int
getNextInt iRef = do
  i <- readSTRef iRef
  modifySTRef iRef (+1)
  return i

-- | Add a labeled vertex using an 'IntGen' to create a fresh label.
addLabeledVertex' :: (MLabeledVertex g, MVertexLabel g ~ Int)
          => g (ST s) -> IntGen s -> ST s Vertex
addLabeledVertex' g ig = addLabeledVertex g =<< getNextInt ig

-- | Add a labeled edge without returning the result (so we don't get warnings).
addLabeledEdge' :: (MLabeledEdge g, PrimMonad m, MonadRef m)
                => g m -> Vertex -> Vertex -> MEdgeLabel g -> m ()
addLabeledEdge' g v1 v2 l = void (addLabeledEdge g v1 v2 l)

-- | Helper function for representing a "COORDINATE" command
coordinate :: Ord s => s -> s -> NFA Int s
coordinate s1 s2 = runST $ do
  g <- newLabeledGraph newMBiDigraph
  ig <- newIntGen

  [q0, q1] <- replicateM 2 (addLabeledVertex' g ig)

  void $ addLabeledEdge g q0 q1 s1
  void $ addLabeledEdge g q1 q0 s2

  mkNFA <$> freeze g <*> pure q0 <*> pure (Set.singleton q0)

-- | [1, 2, 3] --> "(q_1, q_2, q_3)"
showStateLabels :: [Int] -> String
showStateLabels [] = error "showStateLabels called on empty list"
showStateLabels [i] = 'q' : subInt i
showStateLabels is = show' $ ('q':) . subInt <$> is
  where show' s = '(' : (intercalate "," s) ++ ")"

-- | Helper function for 'showStateLabels'
subInt :: Int -> [Char]
subInt i = fromJust . toSub <$> show i

-- | 1 --> "q_1"
showStateLabel :: Int -> String
showStateLabel = showStateLabels . (:[])

-- | Project the vertex labels of an NFA into a singleton list.
listLabels :: Ord sigma => NFA nl sigma -> NFA [nl] sigma
listLabels = nfaMap (:[]) id

newNFAGraph :: (PrimMonad m, MonadRef m)
            => m (LabeledMGraph MBiDigraph nl el m)
newNFAGraph = newLabeledGraph newMBiDigraph

symbols :: Ord s => NFA nl s -> Set s
symbols nfa = Set.fromList [ l | e <- edges (graph nfa)
                               , let Just l = edgeLabel (graph nfa) e]


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

data ATMEvent
  -- User events
  = Insert_Card
  | Identification_Succeeds
  | Request_Withdrawal
  | Get_Money
  | Not_Sufficient_Funds
  | Identification_Fails
  -- ATM events
  | Read_Card
  | Validate_Id
  | Id_Successful
  | Check_Balance
  | Sufficient_Balance
  | Dispense_Money
  | Unsufficient_Balance
  | Id_Failed
  deriving (Show, Read, Eq, Ord)

splitEvents :: ATMEvent -> (Seq ATMEvent, Seq ATMEvent)
splitEvents e = case e of
  Insert_Card -> (Seq.singleton e, Seq.empty)
  Identification_Succeeds -> (Seq.singleton e, Seq.empty)
  Request_Withdrawal -> (Seq.singleton e, Seq.empty)
  Get_Money -> (Seq.singleton e, Seq.empty)
  Not_Sufficient_Funds -> (Seq.singleton e, Seq.empty)
  Identification_Fails -> (Seq.singleton e, Seq.empty)
  _ -> (Seq.empty, Seq.singleton e)

ppEvent :: Show a => a -> String
ppEvent = map (toLower . space) . show
  where space '_' = ' '
        space c = c

ppMerged :: (Show a1, Show a2) => (Seq a2, Seq a1) -> String
ppMerged (Seq.Empty, a Seq.:<| _) = show a
ppMerged (a Seq.:<| _, _) = show a
ppMerged _ = error "ppMerged"

customer :: Int -> NFA Int ATMEvent
customer scope = runST $ do
  g <- newLabeledGraph newMBiDigraph
  ig <- newIntGen

  q0 <- addLabeledVertex' g ig

  startRef <- newSTRef q0
  finalsRef <- newSTRef (Set.singleton q0)

  replicateM_ scope $ do
    start <- readSTRef startRef
    [q1, q2, q3, q4] <- replicateM 4 (addLabeledVertex' g ig)

    addLabeledEdge' g start q1    Insert_Card
    addLabeledEdge' g q1    q2    Identification_Succeeds
    addLabeledEdge' g q1    q4    Identification_Fails
    addLabeledEdge' g q2    q3    Request_Withdrawal
    addLabeledEdge' g q3    q4    Get_Money
    addLabeledEdge' g q3    q4    Not_Sufficient_Funds

    modifySTRef finalsRef (Set.insert q4)
    writeSTRef startRef q4

  mkNFA <$> freeze g <*> pure q0 <*> readSTRef finalsRef

atm :: Int -> NFA Int ATMEvent
atm scope = runST $ do
  g <- newLabeledGraph newMBiDigraph
  ig <- newIntGen

  q0 <- addLabeledVertex' g ig

  startRef <- newSTRef q0
  finalsRef <- newSTRef (Set.singleton q0)

  replicateM_ scope $ do
    start <- readSTRef startRef
    [q1, q2, q3, q4, q5, q6] <- replicateM 6 (addLabeledVertex' g ig)

    addLabeledEdge' g start q1    Read_Card
    addLabeledEdge' g q1    q2    Validate_Id
    addLabeledEdge' g q2    q3    Id_Successful
    addLabeledEdge' g q2    q6    Id_Failed
    addLabeledEdge' g q3    q4    Check_Balance
    addLabeledEdge' g q4    q5    Sufficient_Balance
    addLabeledEdge' g q4    q6    Unsufficient_Balance
    addLabeledEdge' g q5    q6    Dispense_Money

    modifySTRef finalsRef (Set.insert q6)
    writeSTRef startRef q6

  mkNFA <$> freeze g <*> pure q0 <*> readSTRef finalsRef

customer_atm :: Int -> NFA (Seq Int) ATMEvent
customer_atm scope =
  f (customer scope) `join`
  f (coordinate Insert_Card Read_Card) `join`
  f (coordinate Request_Withdrawal Check_Balance) `join`
  f (coordinate Id_Successful Identification_Succeeds) `join`
  f (coordinate Dispense_Money Get_Money) `join`
  f (coordinate Unsufficient_Balance Not_Sufficient_Funds) `join`
  f (coordinate Id_Failed Identification_Fails) `join`
  f (atm scope)

  where f = nfaMap Seq.singleton id

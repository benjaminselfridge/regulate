module Regulate.NFA.Examples.ATM where

import Regulate.NFA.Examples.Helpers
import Regulate.NFA.Haggle

import Control.Monad (replicateM, replicateM_)
import Control.Monad.ST
import Data.Graph.Haggle
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.STRef

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

customer :: Int -> NFA Int ATMEvent
customer scope = runST $ do
  g <- newNFAGraph
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

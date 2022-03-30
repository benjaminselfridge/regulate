{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Regulate.NFA.Mutable
  ( NFABuilder
  , buildNFA
  , MNFA
  , addState
  , addTransition
  , transitionsOut
  , finishNFA
  -- * Regular expression operations
  , symbol
  , union
  ) where

import qualified Regulate.NFA.Haggle as H

import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Graph.Haggle
import Data.Set (Set)
import qualified Data.Set as Set
import Data.STRef

data NFABuilderState s nl sigma =
  NFABuilderState { nbsLabelGen :: STRef s nl
                  , nbsGraph :: LabeledMGraph MBiDigraph nl sigma (ST s)
                  }

type NFABuilder s nl sigma a = StateT (NFABuilderState s nl sigma) (ST s) a

buildNFA :: Ord sigma => nl -> (forall s . MNFA s nl sigma) -> H.NFA nl sigma
buildNFA startLabel mnfa = runST $ do
  lg <- newSTRef startLabel
  newg <- newLabeledGraph newMBiDigraph
  flip evalStateT (NFABuilderState lg newg) $ do
    nfa <- mnfa
    g <- gets nbsGraph
    lift $ H.mkNFA <$> freeze g <*> pure (nfaStart nfa) <*> pure (nfaFinals nfa)

data NFAData = NFAData { nfaStart :: Vertex
                       , nfaFinals :: Set Vertex
                       }

type MNFA s nl sigma = NFABuilder s nl sigma NFAData

getNextLabel :: Enum nl => NFABuilder s nl sigma nl
getNextLabel = do
  lg <- gets nbsLabelGen
  l <- lift $ readSTRef lg
  lift $ modifySTRef lg succ
  return l

-- | Add a new state to the NFA.
addState :: Enum nl => NFABuilder s nl sigma Vertex
addState = do
  g <- gets nbsGraph
  i <- getNextLabel
  v <- lift $ addLabeledVertex g i
  return v

addTransition :: Vertex -> Vertex -> sigma -> NFABuilder s nl sigma ()
addTransition src dst s = do
  g <- gets nbsGraph
  void $ lift $ addLabeledEdge g src dst s

transitionsOut :: Vertex -> NFABuilder s nl sigma [(Vertex, sigma)]
transitionsOut q = do
  g <- gets nbsGraph
  es <- lift $ getOutEdges g q
  forM es $ \e -> do
    s <- lift $ unsafeGetEdgeLabel g e
    return (edgeDest e, s)

finishNFA :: Vertex -> Set Vertex -> MNFA s nl sigma
finishNFA start finals = return $ NFAData start finals

-- | Create an NFA that accepts a single symbol.
symbol :: Enum nl => sigma -> MNFA s nl sigma
symbol s = do
  q0 <- addState
  qf <- addState

  addTransition q0 qf s

  finishNFA q0 (Set.singleton qf)

-- | Take the union of two NFAs.
union :: Enum nl
      => MNFA s nl sigma
      -> MNFA s nl sigma
      -> MNFA s nl sigma
union mnfa1 mnfa2 = do
  nfa1 <- mnfa1
  nfa2 <- mnfa2

  q0 <- addState

  -- For every transition out of either start state, add a transition from q0
  -- to the destination on the same symbol.
  nexts1 <- transitionsOut (nfaStart nfa1)
  nexts2 <- transitionsOut (nfaStart nfa2)
  forM_ (nexts1 ++ nexts2) $ \(q_next, s) -> do
    addTransition q0 q_next s

  finishNFA q0 (nfaFinals nfa1 `Set.union` nfaFinals nfa2)

-- -- | Take the union of two NFAs.
-- union :: Enum nl
--       => MNFA s nl (Maybe sigma)
--       -> MNFA s nl (Maybe sigma)
--       -> MNFA s nl (Maybe sigma)
-- union mnfa1 mnfa2 = do
--   nfa1 <- mnfa1
--   nfa2 <- mnfa2

--   q0 <- addState

--   addTransition q0 (nfaStart nfa1) epsilon
--   addTransition q0 (nfaStart nfa2) epsilon

--   finishNFA q0 (nfaFinals nfa1 `Set.union` nfaFinals nfa2)

-- -- | Concatenate two NFAs.
-- cat :: Enum nl
--     => MNFA s nl (Maybe sigma)
--     -> MNFA s nl (Maybe sigma)
--     -> MNFA s nl (Maybe sigma)
-- cat mnfa1 mnfa2 = do
--   nfa1 <- mnfa1
--   nfa2 <- mnfa2

--   forM_ (nfaFinals nfa1) $ \f -> addTransition f (nfaStart nfa2) epsilon

--   finishNFA (nfaStart nfa1) (nfaFinals nfa2)

-- -- | Start an NFA.
-- star :: Enum nl
--      => MNFA s nl (Maybe sigma)
--      -> MNFA s nl (Maybe sigma)
-- star mnfa = do
--   nfa <- mnfa

--   q0 <- addState

--   addTransition q0 (nfaStart nfa) epsilon
--   forM_ (nfaFinals nfa) $ \f -> addTransition f q0 epsilon

--   finishNFA q0 (Set.insert q0 (nfaFinals nfa))

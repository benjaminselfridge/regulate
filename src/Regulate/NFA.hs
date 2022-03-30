{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Regulate.NFA
  ( NFA, graph, alphabet, startState, finalStates
  , nfaMap
  , NFABuilder
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

import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.Graph.Haggle
import Data.Graph.Haggle.Algorithms.DFS (dfs, rdfs)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.STRef

-- | An @NFA@ over an alphabet @sigma@ is just a directed graph with labels from
-- @sigma@, along with a designated start state and a set of designated final
-- states.
--
-- The vertices of the graph correspond to the states, and the labeled edges are
-- transitions.
data NFA nl sigma = NFA { graph :: LabeledGraph BiDigraph nl sigma
                        , alphabet :: Set sigma
                        , startState :: Vertex
                        , finalStates :: Set Vertex
                        }

data NFABuilderState s nl sigma =
  NFABuilderState { nbsLabelGen :: STRef s nl
                  , nbsGraph :: LabeledMGraph MBiDigraph nl sigma (ST s)
                  }

type NFABuilder s nl sigma a = StateT (NFABuilderState s nl sigma) (ST s) a

buildNFA :: Ord sigma => nl -> (forall s . MNFA s nl sigma) -> NFA nl sigma
buildNFA startLabel mnfa = runST $ do
  lg <- newSTRef startLabel
  newg <- newLabeledGraph newMBiDigraph
  flip evalStateT (NFABuilderState lg newg) $ do
    nfa <- mnfa
    g <- gets nbsGraph
    (g', vertexMap) <- lift $ reduceGraph (nfaStart nfa) (nfaFinals nfa) g
    lift $ mkNFA
      <$> freeze g'
      <*> pure (vertexMap Map.! nfaStart nfa)
      <*> pure (Set.fromList $ (vertexMap Map.!) <$> toList (nfaFinals nfa))

unsafeEdgeLabel :: HasEdgeLabel g => g -> Edge -> EdgeLabel g
unsafeEdgeLabel g e = fromJust (edgeLabel g e)

unsafeVertexLabel :: HasVertexLabel g => g -> Vertex -> VertexLabel g
unsafeVertexLabel g v = fromJust (vertexLabel g v)

mkNFA :: (Ord sigma)
      => LabeledGraph BiDigraph nl sigma
      -> Vertex
      -> Set Vertex
      -> NFA nl sigma
mkNFA g s finals = NFA g (Set.fromList (unsafeEdgeLabel g <$> edges g)) s finals

nfaMap :: (Ord tau)
       => (nl -> nl')
       -> (sigma -> tau)
       -> NFA nl sigma
       -> NFA nl' tau
nfaMap f g nfa = runST $ do
  g' <- newLabeledGraph newMBiDigraph
  vertexMapRef <- newSTRef Map.empty -- map from old vertices to new vertices
  startStateRef <- newSTRef Nothing
  finalStatesRef <- newSTRef Set.empty

  forM_ (vertices (graph nfa)) $ \v -> do
    v' <- addLabeledVertex g' (f $ unsafeVertexLabel (graph nfa) v)
    modifySTRef vertexMapRef (Map.insert v' v)
    when (v == startState nfa) $ writeSTRef startStateRef (Just v')
    when (v `Set.member` finalStates nfa) $
      modifySTRef finalStatesRef (Set.insert v')

  forM_ (edges (graph nfa)) $ \e -> do
    addLabeledEdge g' (edgeSource e) (edgeDest e) (g (unsafeEdgeLabel (graph nfa) e))

  mkNFA
    <$> freeze g'
    <*> (fromJust <$> readSTRef startStateRef)
    <*> readSTRef finalStatesRef

reduceGraph :: Vertex
            -- ^ start state
            -> Set Vertex
            -- ^ final states
            -> LabeledMGraph MBiDigraph nl sigma (ST s)
            -- ^ The graph
            -> ST s ( LabeledMGraph MBiDigraph nl sigma (ST s)
                    , Map Vertex Vertex
                    )
reduceGraph start finals gOld = do
  g' <- freeze gOld
  let reachables = Set.fromList (dfs g' [start])
      finishables = Set.fromList (rdfs g' (Set.toList finals))
      keepStates = reachables `Set.intersection` finishables

  gNew <- newLabeledGraph newMBiDigraph

  -- add all vertices, noting how the old ones map to new ones
  vertexMapRef <- newSTRef Map.empty
  forM_ keepStates $ \v' -> do
    v <- addLabeledVertex gNew (unsafeVertexLabel g' v')
    modifySTRef vertexMapRef (Map.insert v' v)

  vertexMap <- readSTRef vertexMapRef
  -- add all the needed edges
  forM_ keepStates $ \v' -> do
    let vSrc = fromJust $ Map.lookup v' vertexMap
    forM_ (outEdges g' v') $ \e' -> do
      let vDest = fromJust $ Map.lookup (edgeDest e') vertexMap
      when (edgeDest e' `Set.member` keepStates) $
        void $ addLabeledEdge gNew vSrc vDest (unsafeEdgeLabel g' e')

  return (gNew, vertexMap)

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

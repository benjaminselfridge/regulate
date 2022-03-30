{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Regulate.NFA
  ( NFA, graph, alphabet, startState, finalStates
  , NFABuilder
  , Generatable(..)
  , SimpleLabel(..)
  , buildNFA
  , MNFA
  , addState
  , addTransition
  , transitionsOut
  , transitionsIn
  , finishNFA
  -- * Regular expression operations
  , symbol
  , union
  , cat
  , star
  , join
  ) where

import Control.Monad.ST
import Control.Monad.State.Strict hiding (join)
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

class Generatable g where
  type Generator g :: *
  gen :: Generator g -> (g, Generator g)

data SimpleLabel = IntLabel Int | ListLabel [Int]

instance Generatable SimpleLabel where
  type Generator SimpleLabel = Int
  gen i = (IntLabel i, i+1)

instance Semigroup SimpleLabel where
  IntLabel i <> IntLabel j = ListLabel [i, j]
  IntLabel i <> ListLabel js = ListLabel (i:js)
  ListLabel is <> IntLabel j = ListLabel (is++[j])
  ListLabel is <> ListLabel js = ListLabel (is++js)

data NFABuilderState s nl sigma =
  NFABuilderState { nbsLabelGen :: STRef s (Generator nl)
                  , nbsGraph :: LabeledMGraph MBiDigraph nl sigma (ST s)
                  }

type NFABuilder s nl sigma a = StateT (NFABuilderState s nl sigma) (ST s) a

buildNFA :: Ord sigma
         => Generator nl
         -> (forall s . MNFA s nl sigma) -> NFA nl sigma
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

-- TODO: fix this, we don't need to freeze the old one.
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

data NFAData sigma = NFAData { nfaStates :: Set Vertex
                             , nfaTransitions :: Set Edge
                             , nfaStart :: Vertex
                             , nfaFinals :: Set Vertex
                             , nfaAlphabet :: Set sigma
                             }

type MNFA s nl sigma = NFABuilder s nl sigma (NFAData sigma)

getNextLabel :: Generatable nl => NFABuilder s nl sigma nl
getNextLabel = do
  lgRef <- gets nbsLabelGen
  lg <- lift $ readSTRef lgRef
  let (l', lg') = gen lg
  lift $ writeSTRef lgRef lg'
  return l'

-- | Add a new state to the NFA, generating a new label for it.
addState :: Generatable nl => NFABuilder s nl sigma Vertex
addState = do
  g <- gets nbsGraph
  i <- getNextLabel
  v <- lift $ addLabeledVertex g i
  return v

-- | Add a new state to the NFA with particular label.
addStateWithLabel :: nl -> NFABuilder s nl sigma Vertex
addStateWithLabel nl = do
  g <- gets nbsGraph
  v <- lift $ addLabeledVertex g nl
  return v

addTransition :: Vertex -> Vertex -> sigma -> NFABuilder s nl sigma Edge
addTransition src dst s = do
  g <- gets nbsGraph
  lift $ fromJust <$> addLabeledEdge g src dst s

transitionsIn :: Vertex -> NFABuilder s nl sigma [(Vertex, sigma)]
transitionsIn q = do
  g <- gets nbsGraph
  es <- lift $ getInEdges g q
  forM es $ \e -> do
    s <- lift $ unsafeGetEdgeLabel g e
    return (edgeSource e, s)

transitionsOut :: Vertex -> NFABuilder s nl sigma [(Vertex, sigma)]
transitionsOut q = do
  g <- gets nbsGraph
  es <- lift $ getOutEdges g q
  forM es $ \e -> do
    s <- lift $ unsafeGetEdgeLabel g e
    return (edgeDest e, s)

stateLabel :: Vertex -> NFABuilder s nl sigma nl
stateLabel v = do
  g <- gets nbsGraph
  lift $ fromJust <$> getVertexLabel g v

transitionSymbol :: Edge -> NFABuilder s nl sigma sigma
transitionSymbol e = do
  g <- gets nbsGraph
  lift $ fromJust <$> getEdgeLabel g e

finishNFA :: Set Vertex
          -> Set Edge
          -> Vertex
          -> Set Vertex
          -> Set sigma
          -> MNFA s nl sigma
finishNFA states transitions start finals alpha =
  return $ NFAData states transitions start finals alpha

-- | Create an NFA that accepts a single symbol.
symbol :: Generatable nl => sigma -> MNFA s nl sigma
symbol s = do
  q0 <- addState
  qf <- addState

  t <- addTransition q0 qf s

  finishNFA
    (Set.fromList [q0, qf])
    (Set.singleton t)
    q0
    (Set.singleton qf)
    (Set.singleton s)

-- | Take the union of two NFAs.
union :: (Generatable nl, Ord sigma)
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
  ts <- forM (nexts1 ++ nexts2) $ \(q_next, s) -> do
    addTransition q0 q_next s

  finishNFA
    (Set.insert q0 (nfaStates nfa1 `Set.union` nfaStates nfa2))
    ( Set.fromList ts `Set.union`
      nfaTransitions nfa1 `Set.union`
      nfaTransitions nfa2 )
    q0
    (nfaFinals nfa1 `Set.union` nfaFinals nfa2)
    (nfaAlphabet nfa1 `Set.union` nfaAlphabet nfa2)

-- | Concatenate two NFAs.
cat :: (Generatable nl, Ord sigma)
    => MNFA s nl sigma
    -> MNFA s nl sigma
    -> MNFA s nl sigma
cat mnfa1 mnfa2 = do
  nfa1 <- mnfa1
  nfa2 <- mnfa2

  -- For every transition to a final state of nfa1, add a transition from the
  -- source to the start state of nfa2.
  ts <- forM (toList (nfaFinals nfa1)) $ \f1 -> do
    preds <- transitionsIn f1
    forM (toList preds) $ \(q_previous, s) -> do
      addTransition q_previous (nfaStart nfa2) s

  finishNFA
    (nfaStates nfa1 `Set.union` nfaStates nfa2)
    ( Set.fromList (concat ts) `Set.union`
      nfaTransitions nfa1 `Set.union`
      nfaTransitions nfa2 )
    (nfaStart nfa1)
    (nfaFinals nfa2)
    (nfaAlphabet nfa1 `Set.union` nfaAlphabet nfa2)

-- | Star an NFA.
star :: (Generatable nl, Ord sigma)
     => MNFA s nl sigma
     -> MNFA s nl sigma
star mnfa = do
  nfa <- mnfa

  q0 <- addState

  -- For every transition out the start state, add a transition from q0
  -- to the destination on the same symbol.
  nexts <- transitionsOut (nfaStart nfa)
  ts1 <- forM (toList nexts) $ \(q_next, s) -> do
    addTransition q0 q_next s

  -- For every transition to a final state, add a transition from the source to
  -- q0.
  ts2 <- forM (toList (nfaFinals nfa)) $ \f -> do
    preds <- transitionsIn f
    forM (toList preds) $ \(q_previous, s) -> do
      addTransition q_previous q0 s

  -- Our new set of final states is just q0.
  finishNFA
    (Set.insert q0 (nfaStates nfa))
    ( Set.fromList ts1 `Set.union`
      Set.fromList (concat ts2) `Set.union`
      nfaTransitions nfa )
    q0
    (Set.singleton q0)
    (nfaAlphabet nfa)

-- | Join two NFAs on their shared symbols.

-- | Given two NFA nls that operate on a common subset, produce an NFA nl whose
-- alphabet is the union of the two alphabets, and whose language is exactly the
-- strings that, when restricted to either of the two alphabets, are exactly the
-- corresponding languages.
join :: (Generatable nl, Semigroup nl, Ord sigma)
     => MNFA s nl sigma -> MNFA s nl sigma -> MNFA s nl sigma
join mnfa1 mnfa2 = do
  nfa1 <- mnfa1
  nfa2 <- mnfa2

  startRef <- lift $ newSTRef Nothing
  finalsRef <- lift $ newSTRef Set.empty

  -- map from (nfa1, nfa2) state pairs to joined states.
  stateMapRef <- lift $ newSTRef Map.empty

  -- First, add all the new vertices and build up the vertex map.
  states <- forM (toList $ nfaStates nfa1) $ \q1 -> do
    forM (toList $ nfaStates nfa2) $ \q2 -> do
      l <- (<>) <$> stateLabel q1 <*> stateLabel q2
      q <- addStateWithLabel l

      -- Add new state to vertex map
      lift $ modifySTRef stateMapRef (Map.insert (q1, q2) q)

      -- If both states are the starting state, then the new state is the new
      -- starting state.
      when (q1 == nfaStart nfa1 && q2 == nfaStart nfa2) $
        lift $ writeSTRef startRef (Just q)

      -- If both states are final states, then the new state is a final state.
      when (q1 `Set.member` nfaFinals nfa1 && q2 `Set.member` nfaFinals nfa2) $
        lift $ modifySTRef finalsRef (Set.insert q)

      return q

  stateMap <- lift $ readSTRef stateMapRef

  let sharedSymbols = nfaAlphabet nfa1 `Set.intersection` nfaAlphabet nfa2

  -- Create a map from all shared labels to corresponding edges in nfa2.
  let p m t = do
        a <- transitionSymbol t
        if a `Set.member` sharedSymbols
          then return $ Map.insertWith Set.union a (Set.singleton t) m
          else return m
  nfa2SharedSymbolEdgeMap <- foldM p Map.empty (nfaTransitions nfa2)

  ts1 <- forM (toList $ nfaTransitions nfa1) $ \t -> do
    a <- transitionSymbol t
    case a `Set.member` sharedSymbols of
      False -> forM (toList $ nfaStates nfa2) $ \q2 -> do
        let src = stateMap Map.! (edgeSource t, q2)
            dst = stateMap Map.! (edgeDest t, q2)
        addTransition src dst a
      True -> case Map.lookup a nfa2SharedSymbolEdgeMap of
        Nothing -> return []
        Just ts -> forM (toList ts) $ \t' -> do
          let src = stateMap Map.! (edgeSource t, edgeSource t')
              dst = stateMap Map.! (edgeDest t, edgeDest t')
          addTransition src dst a

  ts2 <- forM (toList $ nfaTransitions nfa2) $ \t -> do
    a <- transitionSymbol t
    case a `Set.member` sharedSymbols of
      True -> return []
      False  -> forM (toList $ nfaStates nfa1) $ \q1 -> do
        let src = stateMap Map.! (q1, edgeSource t)
            dst = stateMap Map.! (q1, edgeDest t)
        addTransition src dst a

  start <- lift $ fromJust <$> readSTRef startRef
  finals <- lift $ readSTRef finalsRef

  finishNFA
    (Set.fromList (concat states))
    (Set.fromList (concat (ts1 ++ ts2)))
    start
    finals
    (nfaAlphabet nfa1 `Set.union` nfaAlphabet nfa2)

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Regulate.NFA
  ( NFA, graph, alphabet, startState, finalStates
  , NFABuilder
  , buildNFA
  , MNFA
  , addState
  , addTransition
  , transitionsOut
  , transitionsIn
  , finishNFA
  -- * Regular expression operations
  , empty
  , epsilon
  , symbol
  , string
  , union
  , cat
  , pow
  , upto
  , star
  , join
  -- * Generating strings
  , generate
  , generateAll
  ) where

import Control.Monad.ST
import Control.Monad.State.Strict hiding (join)
import Data.Foldable (toList)
import Data.Graph.Haggle
import Data.Graph.Haggle.Algorithms.DFS (dfs, rdfs)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, catMaybes)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.STRef

-- | An @NFA@ over an alphabet @sigma@ is just a directed graph with labels from
-- @sigma@, along with a designated start state and a set of designated final
-- states.
--
-- The vertices of the graph correspond to the states, and the labeled edges are
-- transitions.
data NFA sigma = NFA { graph :: EdgeLabeledGraph BiDigraph sigma
                     , alphabet :: Set sigma
                     , startState :: Vertex
                     , finalStates :: Set Vertex
                     }

data NFABuilderState s sigma =
  NFABuilderState { nbsGraph :: EdgeLabeledMGraph MBiDigraph sigma (ST s) }

type NFABuilder s sigma a = StateT (NFABuilderState s sigma) (ST s) a

buildNFA :: Ord sigma
         => (forall s . MNFA s sigma) -> NFA sigma
buildNFA mnfa = runST $ do
  newg <- newEdgeLabeledGraph newMBiDigraph
  flip evalStateT (NFABuilderState newg) $ do
    nfa <- mnfa
    g <- gets nbsGraph
    -- lift $ mkNFA
    --   <$> freeze g
    --   <*> pure (nfaStart nfa)
    --   <*> pure (nfaFinals nfa)
    (g', vertexMap) <- lift $ reduceGraph (nfaStart nfa) (nfaFinals nfa) g
    lift $ mkNFA
      <$> freeze g'
      <*> pure (vertexMap Map.! nfaStart nfa)
      <*> pure (Set.fromList . catMaybes $ (flip Map.lookup vertexMap) <$> toList (nfaFinals nfa))

unsafeEdgeLabel :: HasEdgeLabel g => g -> Edge -> EdgeLabel g
unsafeEdgeLabel g e = fromJust (edgeLabel g e)

mkNFA :: (Ord sigma)
      => EdgeLabeledGraph BiDigraph sigma
      -> Vertex
      -> Set Vertex
      -> NFA sigma
mkNFA g s finals = NFA g (Set.fromList (unsafeEdgeLabel g <$> edges g)) s finals

-- TODO: fix this, we don't need to freeze the old one.
reduceGraph :: Vertex
            -- ^ start state
            -> Set Vertex
            -- ^ final states
            -> EdgeLabeledMGraph MBiDigraph sigma (ST s)
            -- ^ The graph
            -> ST s ( EdgeLabeledMGraph MBiDigraph sigma (ST s)
                    , Map Vertex Vertex
                    )
reduceGraph start finals gOld = do
  g' <- freeze gOld
  let reachables = Set.fromList (dfs g' [start])
      finishables = Set.fromList (rdfs g' (Set.toList finals))
      keepStates = Set.insert start (reachables `Set.intersection` finishables)

  gNew <- newEdgeLabeledGraph newMBiDigraph

  -- add all vertices, noting how the old ones map to new ones
  vertexMapRef <- newSTRef Map.empty
  forM_ keepStates $ \v' -> do
    v <- addVertex gNew
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

type MNFA s sigma = NFABuilder s sigma (NFAData sigma)

-- | Add a new state to the NFA, generating a new label for it.
addState :: NFABuilder s sigma Vertex
addState = do
  g <- gets nbsGraph
  lift $ addVertex g

addTransition :: Vertex -> Vertex -> sigma -> NFABuilder s sigma Edge
addTransition src dst s = do
  g <- gets nbsGraph
  lift $ fromJust <$> addLabeledEdge g src dst s

transitionsIn :: Vertex -> NFABuilder s sigma [(Vertex, sigma)]
transitionsIn q = do
  g <- gets nbsGraph
  es <- lift $ getInEdges g q
  forM es $ \e -> do
    s <- lift $ unsafeGetEdgeLabel g e
    return (edgeSource e, s)

transitionsOut :: Vertex -> NFABuilder s sigma [(Vertex, sigma)]
transitionsOut q = do
  g <- gets nbsGraph
  es <- lift $ getOutEdges g q
  forM es $ \e -> do
    s <- lift $ unsafeGetEdgeLabel g e
    return (edgeDest e, s)

transitionSymbol :: Edge -> NFABuilder s sigma sigma
transitionSymbol e = do
  g <- gets nbsGraph
  lift $ fromJust <$> getEdgeLabel g e

finishNFA :: Set Vertex
          -- ^ All the states in this NFA
          -> Set Edge
          -- ^ All the edges in this NFA
          -> Vertex
          -- ^ Starting state
          -> Set Vertex
          -- ^ Final states
          -> Set sigma
          -- ^ alphabet
          -> MNFA s sigma
finishNFA states transitions start finals alpha =
  return $ NFAData states transitions start finals alpha

-- | Create an NFA that accepts no strings.
empty :: MNFA s sigma
empty = do
  q0 <- addState
  finishNFA (Set.singleton q0) Set.empty q0 Set.empty Set.empty

-- | Create an NFA that accepts only the empty string.
epsilon :: MNFA s sigma
epsilon = do
  q0 <- addState
  finishNFA (Set.singleton q0) Set.empty q0 (Set.singleton q0) Set.empty

-- | Create an NFA that accepts a single symbol.
symbol :: sigma -> MNFA s sigma
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

-- | Create an NFA that accepts a particular string.
string :: Ord sigma => [sigma] -> MNFA s sigma
string [] = epsilon
string (a:as) = symbol a `cat` string as

-- | Take the union of two NFAs.
union :: (Ord sigma)
      => MNFA s sigma
      -> MNFA s sigma
      -> MNFA s sigma
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
    (nfaFinals nfa1 `Set.union` nfaFinals nfa2 `Set.union`
     if nfaStart nfa1 `Set.member` nfaFinals nfa1 ||
        nfaStart nfa2 `Set.member` nfaFinals nfa2
     then Set.singleton q0
     else Set.empty
    )
    (nfaAlphabet nfa1 `Set.union` nfaAlphabet nfa2)

-- | Concatenate two NFAs.
cat :: Ord sigma
    => MNFA s sigma
    -> MNFA s sigma
    -> MNFA s sigma
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
    (Set.fromList (concat ts) `Set.union`
     nfaTransitions nfa1 `Set.union`
     nfaTransitions nfa2)
    (nfaStart nfa1)
    (nfaFinals nfa2 `Set.union`
     if nfaStart nfa1 `Set.member` nfaFinals nfa1
     then Set.singleton (nfaStart nfa1)
     else Set.empty)
    (nfaAlphabet nfa1 `Set.union` nfaAlphabet nfa2)

-- | Repeat an NFA some number of times.
pow :: Ord sigma => Int -> MNFA s sigma -> MNFA s sigma
pow i _ | i <= 0 = epsilon
pow i mnfa = mnfa `cat` (pow (i-1) mnfa)

upto :: Ord sigma => Int -> MNFA s sigma -> MNFA s sigma
upto i _ | i <= 0 = epsilon
upto i mnfa = mnfa `cat` (epsilon `union` upto (i-1) mnfa)

-- | Star an NFA.
star :: Ord sigma
     => MNFA s sigma
     -> MNFA s sigma
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

-- | Given two NFAs that operate on a common subset, produce an NFA nl whose
-- alphabet is the union of the two alphabets, and whose language is exactly the
-- strings that, when restricted to either of the two alphabets, are exactly the
-- corresponding languages.
--
-- When the alphabets are the same, this is the same as intersection.
join :: Ord sigma
     => MNFA s sigma -> MNFA s sigma -> MNFA s sigma
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
      q <- addState

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

-- | One round of string generation.
generate1 :: (Monoid m, Ord m, Graph g)
          => (sigma -> m)
          -- ^ map from symbols into some monoid
          -> EdgeLabeledGraph g sigma
          -- ^ The NFAraph
          -> Set (Vertex, m)
          -- ^ set of discovered (Vertex, m) pairs thus far
          -> Seq (Vertex, m)
          -- ^ (Vertex, m) pairs to generate from
          -> (Set (Vertex, m), Seq (Vertex, m))
          -- ^ The new set (with the newly-discovered pairs), plus the new pairs
          -- that we need to generate from next
generate1 _ _ seen Seq.Empty = (seen, Seq.Empty)
generate1 f g seen ((v, s) Seq.:<| from) =
  let new =  [ (d, s <> f l) | e <- outEdges g v
                             , let d = edgeDest e
                             , let Just l = edgeLabel g e ]
  in ( seen `Set.union` Set.fromList new
     , from Seq.>< [ pair | pair <- Seq.fromList new, (not . (`elem` seen)) pair ]
     )

-- | An infinite list generated by 'iterate'ing the 'generate1' function.
generate' :: (Monoid m, Ord m, Graph g)
          => (sigma -> m)
          -- ^ map from symbols into some monoid
          -> EdgeLabeledGraph g sigma
          -- ^ The NFA nl graph
          -> Set (Vertex, m)
          -- ^ set of discovered (Vertex, string) pairs thus far
          -> Seq (Vertex, m)
          -- ^ (Vertex, string) pairs to generate from
          -> [(Set (Vertex, m), Seq (Vertex, m))]
          -- ^ The new set (with the newly-discovered pairs), plus the new pairs
          -- that we need to generate from next
generate' f g seen from = iterate (uncurry (generate1 f g)) (seen, from)

-- | Generate strings from an NFA nl. The 'Int' is the number of times to walk the
-- NFA nl graph. If there are multiple final states that accept the same string,
-- that string will appear (correspondingly) multiple times in this list.
generate :: (Monoid m, Ord m)
         => (sigma -> m)
         -- ^ map from symbols into some monoid
         -> Int
         -- ^ How many rounds of generation to perform
         -> NFA sigma
         -> [m]
generate f n nfa =
  [ s | let (pairs, _) = generate' f (graph nfa) (Set.singleton (startState nfa, mempty)) (Seq.singleton (startState nfa, mempty)) !! n
      , (q, s) <- Set.toList pairs
      , q `Set.member` finalStates nfa
      ]

-- | Generate the entire list of strings from an NFA. This is an exhaustive
-- breadth-first search that only stops when it cannot generate new strings;
-- therefore if the NFA has cycles, it may never terminate.
generateAll :: (Monoid m, Ord m) => (sigma -> m) -> NFA sigma -> [m]
generateAll f nfa =
  [ s | let (pairs, _) = term $ generate' f (graph nfa) (Set.singleton (startState nfa, mempty)) (Seq.singleton (startState nfa, mempty))
      , (q, s) <- Set.toList pairs
      , q `Set.member` finalStates nfa
      ]

  where term (a:a':as) | a == a' = a
                       | otherwise = term (a':as)
        term _ = error "generateAll"

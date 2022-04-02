{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Implementation of non-deterministic finite automata.
--
-- Clients of this API are encouraged to build up NFAs using the regular
-- expression operations, but manual construction is also possible via the
-- 'NFABuilder' monad and corresponding functions.
module Regulate.NFA
  (
    -- * NFAs
    NFA, graph, alphabet, startState, finalStates
    -- * Constructing NFAs
  , NFABuilder
  , NFABuilderState
  , buildNFA
  , MNFA
  , NFAData(..)
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
                       -- ^ The NFA's underling @haggle@ graph.
                     , alphabet :: Set sigma
                       -- ^ The NFA's alphabet.
                     , startState :: Vertex
                       -- ^ Starting state.
                     , finalStates :: Set Vertex
                       -- ^ Final states.
                     }

mkNFA :: (Ord sigma)
      => EdgeLabeledGraph BiDigraph sigma
      -> Vertex
      -> Set Vertex
      -> NFA sigma
mkNFA g s finals = NFA g (Set.fromList (unsafeEdgeLabel g <$> edges g)) s finals

-- | The state for the 'NFABuilder' monad. Don't mess with this directly.
data NFABuilderState s sigma =
  NFABuilderState { nbsGraph :: EdgeLabeledMGraph MBiDigraph sigma (ST s) }

-- | Monad for building NFAs.
type NFABuilder s sigma a = StateT (NFABuilderState s sigma) (ST s) a

-- | Basic data for an NFA.
data NFAData sigma = NFAData { nfaStates :: Set Vertex
                             , nfaTransitions :: Set Edge
                             , nfaStart :: Vertex
                             , nfaFinals :: Set Vertex
                             , nfaAlphabet :: Set sigma
                             }

-- | A constructed, mutable MNFA.
type MNFA s sigma = NFABuilder s sigma (NFAData sigma)

-- | Run an MNFA action to produce a completed NFA. This also reduces the
-- resulting graph, getting rid of unreachable/unfinishable states.
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

-- | Remove unreachable and unfinishable nodes (always preserving the starting
-- node), and remove all duplicate edges (edges with the same label, source, and
-- destination vertices).
reduceGraph :: Ord sigma
            => Vertex
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
  edgeSetRef <- newSTRef Set.empty
  -- add all the needed edges
  forM_ keepStates $ \v' -> do
    let vSrc = fromJust $ Map.lookup v' vertexMap
    forM_ (outEdges g' v') $ \e' -> do
      let vDest = fromJust $ Map.lookup (edgeDest e') vertexMap
      edgeSet <- readSTRef edgeSetRef
      let l = unsafeEdgeLabel g' e'
      when (edgeDest e' `Set.member` keepStates &&
            (l, vSrc, vDest) `Set.notMember` edgeSet) $ do
        void $ addLabeledEdge gNew vSrc vDest (unsafeEdgeLabel g' e')
        modifySTRef edgeSetRef (Set.insert (l, vSrc, vDest))

  return (gNew, vertexMap)

reduceNFA :: Ord sigma => MNFA s sigma -> MNFA s sigma
reduceNFA mnfa = do
  nfa <- mnfa
  g <- gets nbsGraph
  (g', vertexMap) <- lift $ reduceGraph (nfaStart nfa) (nfaFinals nfa) g
  states <- lift $ getVertices g'
  transitions <- lift $ concat <$> mapM (getOutEdges g') states
  return $ NFAData
    (Set.fromList states)
    (Set.fromList transitions)
    (vertexMap Map.! nfaStart nfa)
    (Set.fromList . catMaybes $ (flip Map.lookup vertexMap) <$> toList (nfaFinals nfa))
    (nfaAlphabet nfa)

-- | Add a new state to the NFA graph.
addState :: NFABuilder s sigma Vertex
addState = do
  g <- gets nbsGraph
  lift $ addVertex g

-- | Add a new transition to the NFA graph.
addTransition :: Vertex -> Vertex -> sigma -> NFABuilder s sigma Edge
addTransition src dst s = do
  g <- gets nbsGraph
  lift $ fromJust <$> addLabeledEdge g src dst s

-- | Given a state, get a list of all the states it can transition to, along
-- with the corresponding transition symbol.
transitionsIn :: Vertex -> NFABuilder s sigma [(Vertex, sigma)]
transitionsIn q = do
  g <- gets nbsGraph
  es <- lift $ getInEdges g q
  forM es $ \e -> do
    s <- lift $ unsafeGetEdgeLabel g e
    return (edgeSource e, s)

-- | Given a state, get a list of all the states that can transition to it,
-- along with the corresponding transition symbol.
transitionsOut :: Vertex -> NFABuilder s sigma [(Vertex, sigma)]
transitionsOut q = do
  g <- gets nbsGraph
  es <- lift $ getOutEdges g q
  forM es $ \e -> do
    s <- lift $ unsafeGetEdgeLabel g e
    return (edgeDest e, s)

-- | Given a transition, get that transition's symbol.
transitionSymbol :: Edge -> NFABuilder s sigma sigma
transitionSymbol e = do
  g <- gets nbsGraph
  lift $ fromJust <$> getEdgeLabel g e

-- | Finish constructing an NFA. You must manually add all of the states that
-- belong to this NFA; this function does not check that!
finishNFA :: Ord sigma
          => Set Vertex
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
empty :: Ord sigma => MNFA s sigma
empty = do
  q0 <- addState
  finishNFA (Set.singleton q0) Set.empty q0 Set.empty Set.empty

-- | Create an NFA that accepts only the empty string.
epsilon :: Ord sigma => MNFA s sigma
epsilon = do
  q0 <- addState
  finishNFA (Set.singleton q0) Set.empty q0 (Set.singleton q0) Set.empty

-- | Create an NFA that accepts a single symbol.
symbol :: Ord sigma => sigma -> MNFA s sigma
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
union :: Ord sigma
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
  -- source to the start state of nfa2 on the same symbol.
  ts1 <- forM (toList (nfaFinals nfa1)) $ \f1 -> do
    preds <- transitionsIn f1
    forM (toList preds) $ \(q_previous, s) -> do
      addTransition q_previous (nfaStart nfa2) s

  -- If the start state of nfa1 is a final state, add transitions from nfa1's
  -- start state to all the successors of nfa2's start state, using the same
  -- symbol from nfa2's start to the successor.
  ts2 <- if (nfaStart nfa1 `Set.member` nfaFinals nfa1)
         then do succs <- transitionsOut (nfaStart nfa2)
                 forM (toList succs) $ \(q_succ, s) -> do
                   addTransition (nfaStart nfa1) q_succ s
         else return []

  finishNFA
    (nfaStates nfa1 `Set.union` nfaStates nfa2)
    (Set.fromList (concat ts1) `Set.union`
     Set.fromList ts2 `Set.union`
     nfaTransitions nfa1 `Set.union`
     nfaTransitions nfa2)
    (nfaStart nfa1)
    (nfaFinals nfa2 `Set.union`
     if nfaStart nfa1 `Set.member` nfaFinals nfa1 &&
        nfaStart nfa2 `Set.member` nfaFinals nfa2
     then Set.singleton (nfaStart nfa1)
     else Set.empty)
    (nfaAlphabet nfa1 `Set.union` nfaAlphabet nfa2)

-- | Repeat an NFA some number of times.
pow :: Ord sigma => Int -> MNFA s sigma -> MNFA s sigma
pow i _ | i <= 0 = epsilon
pow i mnfa = mnfa `cat` (pow (i-1) mnfa)

-- | Like 'pow', but accepts w^0, w^1, ..., w^n.
upto :: Ord sigma => Int -> MNFA s sigma -> MNFA s sigma
upto i _ | i <= 0 = epsilon
upto i mnfa = epsilon `union` (mnfa `cat` upto (i-1) mnfa)

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

-- | Join two NFAs on their shared symbols. Given two NFAs that operate on a
-- common subset, produce an NFA whose alphabet is the union of the two
-- alphabets, and whose language is exactly the strings that, when restricted to
-- either of the two alphabets, are exactly the corresponding languages.
--
-- When the alphabets are the same, this is the same as intersection.

join :: Ord sigma => NFA sigma -> NFA sigma -> NFA sigma
join nfa1 nfa2 = runST $ do
  let g1 = graph nfa1
      g2 = graph nfa2
      s1 = startState nfa1
      s2 = startState nfa2
      finals1 = finalStates nfa1
      finals2 = finalStates nfa2

  g <- newEdgeLabeledGraph newMBiDigraph
  startRef <- newSTRef Nothing
  finalsRef <- newSTRef Set.empty

  -- map from (g1, g2) vertex pairs to g vertices.
  vertexMapRef <- newSTRef Map.empty

  -- First, add all the vertices and build up the vertex map.
  forM_ (vertices g1) $ \v1 -> do
    forM_ (vertices g2) $ \v2 -> do
      v <- addVertex g
      -- Add new vertex to vertex map
      modifySTRef vertexMapRef (Map.insert (v1, v2) v)
      -- If both vertices are the start state, then the new vertex is the joined
      -- start state
      when (v1 == s1 && v2 == s2) $
        writeSTRef startRef (Just v)
      -- If both vertices are final states, then the new vertex is one of the
      -- joined final states
      when (v1 `Set.member` finals1 && v2 `Set.member` finals2) $
        modifySTRef finalsRef (Set.insert v)

  vertexMap <- readSTRef vertexMapRef

  let sharedSymbols = alphabet nfa1 `Set.intersection` alphabet nfa2

  -- Create a map from all shared labels to corresponding edges in g2.
  let g2LeftEdgeMap = foldr p Map.empty (edges g2)
      p e m = let a = fromJust (edgeLabel g2 e)
              in if a `Set.member` sharedSymbols
                 then Map.insertWith Set.union a (Set.singleton e) m
                 else m

  forM_ (edges g1) $ \e -> do
    let a = fromJust (edgeLabel g1 e)
    case a `Set.member` sharedSymbols of
      False -> forM_ (vertices g2) $ \v2 -> do
        let src = vertexMap Map.! (edgeSource e, v2)
            dst = vertexMap Map.! (edgeDest e, v2)
        addLabeledEdge g src dst a
      True -> case Map.lookup a g2LeftEdgeMap of
        -- Look up the g2 edges with the same label, and add the corresponding
        -- vertices.
        Nothing -> return ()
        Just es -> forM_ es $ \e' -> do
          let src = vertexMap Map.! (edgeSource e, edgeSource e')
              dst = vertexMap Map.! (edgeDest e, edgeDest e')
          addLabeledEdge g src dst a

  forM_ (edges g2) $ \e -> do
    let a = fromJust (edgeLabel g2 e)
    case a `Set.member` sharedSymbols of
      True -> return ()
      False  -> forM_ (vertices g1) $ \v1 -> do
        let src = vertexMap Map.! (v1, edgeSource e)
            dst = vertexMap Map.! (v1, edgeDest e)
        addLabeledEdge g src dst a

  start <- readSTRef startRef
  finals <- readSTRef finalsRef

  (g'Mut, reduceMap) <- reduceGraph (fromJust start) finals g
  g' <- freeze g'Mut
  let start' = fromJust (Map.lookup (fromJust start) reduceMap)
      finals' = Set.fromList (catMaybes (flip Map.lookup reduceMap <$> Set.toList finals))
  return $ mkNFA g' start' finals'

-- join :: Ord sigma
--      => (forall s1 . MNFA s1 sigma)
--      -> (forall s2 . MNFA s2 sigma)
--      -> MNFA s sigma
-- join mnfa1 mnfa2 = do
--   nfa1 <- mnfa1
--   nfa2 <- mnfa2

--   startRef <- lift $ newSTRef Nothing
--   finalsRef <- lift $ newSTRef Set.empty

--   -- map from (nfa1, nfa2) state pairs to joined states.
--   stateMapRef <- lift $ newSTRef Map.empty

--   -- First, add all the new vertices and build up the vertex map.
--   states <- forM (toList $ nfaStates nfa1) $ \q1 -> do
--     forM (toList $ nfaStates nfa2) $ \q2 -> do
--       q <- addState

--       -- Add new state to vertex map
--       lift $ modifySTRef stateMapRef (Map.insert (q1, q2) q)

--       -- If both states are the starting state, then the new state is the new
--       -- starting state.
--       when (q1 == nfaStart nfa1 && q2 == nfaStart nfa2) $
--         lift $ writeSTRef startRef (Just q)

--       -- If both states are final states, then the new state is a final state.
--       when (q1 `Set.member` nfaFinals nfa1 && q2 `Set.member` nfaFinals nfa2) $
--         lift $ modifySTRef finalsRef (Set.insert q)

--       return q

--   stateMap <- lift $ readSTRef stateMapRef

--   let sharedSymbols = nfaAlphabet nfa1 `Set.intersection` nfaAlphabet nfa2

--   -- Create a map from all shared labels to corresponding edges in nfa2.
--   let p m t = do
--         a <- transitionSymbol t
--         if a `Set.member` sharedSymbols
--           then return $ Map.insertWith Set.union a (Set.singleton t) m
--           else return m
--   nfa2SharedSymbolEdgeMap <- foldM p Map.empty (nfaTransitions nfa2)

--   ts1 <- forM (toList $ nfaTransitions nfa1) $ \t -> do
--     a <- transitionSymbol t
--     case a `Set.member` sharedSymbols of
--       False -> forM (toList $ nfaStates nfa2) $ \q2 -> do
--         let src = stateMap Map.! (edgeSource t, q2)
--             dst = stateMap Map.! (edgeDest t, q2)
--         addTransition src dst a
--       True -> case Map.lookup a nfa2SharedSymbolEdgeMap of
--         Nothing -> return []
--         Just ts -> forM (toList ts) $ \t' -> do
--           let src = stateMap Map.! (edgeSource t, edgeSource t')
--               dst = stateMap Map.! (edgeDest t, edgeDest t')
--           addTransition src dst a

--   ts2 <- forM (toList $ nfaTransitions nfa2) $ \t -> do
--     a <- transitionSymbol t
--     case a `Set.member` sharedSymbols of
--       True -> return []
--       False  -> forM (toList $ nfaStates nfa1) $ \q1 -> do
--         let src = stateMap Map.! (q1, edgeSource t)
--             dst = stateMap Map.! (q1, edgeDest t)
--         addTransition src dst a

--   start <- lift $ fromJust <$> readSTRef startRef
--   finals <- lift $ readSTRef finalsRef

--   finishNFA
--     (Set.fromList (concat states))
--     (Set.fromList (concat (ts1 ++ ts2)))
--     start
--     finals
--     (nfaAlphabet nfa1 `Set.union` nfaAlphabet nfa2)

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

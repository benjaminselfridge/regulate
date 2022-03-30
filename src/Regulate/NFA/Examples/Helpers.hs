{-# LANGUAGE GADTs #-}

module Regulate.NFA.Examples.Helpers where

import Regulate.NFA.Haggle

import Control.Monad (void, replicateM)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef)
import Control.Monad.ST
import Data.Char (toLower)
import Data.Char.Small (toSub)
import Data.Graph.Haggle
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
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


ppEvent :: Show a => a -> String
ppEvent = map (toLower . space) . show
  where space '_' = ' '
        space c = c

ppMerged :: (Show a1, Show a2) => (Seq a2, Seq a1) -> String
ppMerged (Seq.Empty, a Seq.:<| _) = show a
ppMerged (a Seq.:<| _, _) = show a
ppMerged _ = error "ppMerged"

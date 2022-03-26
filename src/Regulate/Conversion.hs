-- | Module for converting between various representations of regular languages.
module Regulate.Conversion where

-- import Control.Monad.ST
-- import Regulate.NFA ( NFA(..) )
-- import Regulate.Regex ( Regex(..) )

-- import Data.Graph.Haggle
-- import Data.Set (Set)
-- import qualified Data.Set as Set

-- data Tree a = Leaf a
--             | Branch (Tree a) (Tree a)

-- regexToNFA' :: Regex sigma
--             -> ST s ( Tree (EdgeLabeledMGraph MDigraph (Maybe sigma) (ST s))
--                     , Vertex
--                     , Set Vertex)
-- regexToNFA' EmptySet = do
--   g <- newEdgeLabeledGraph newMDigraph
--   q0 <- addVertex g
--   return $ (Leaf g, q0, Set.empty)
-- regexToNFA' EmptyString = do
--   g <- newEdgeLabeledGraph newMDigraph
--   q0 <- addVertex g
--   return $ (Leaf g, q0, Set.singleton q0)
-- regexToNFA' (Constant a) = do
--   g <- newEdgeLabeledGraph newMDigraph
--   q0 <- addVertex g
--   q1 <- addVertex g
--   addLabeledEdge g q0 q1 (Just a)
--   return $ (Leaf g, q0, Set.singleton q1)
-- regexToNFA' (Alt r s) = do
--   undefined
-- regexToNFA' (Cat r s) = undefined
-- regexToNFA' (Plus r) = undefined

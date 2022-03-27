{-# LANGUAGE TupleSections #-}

-- | Module for rendering NFAs via GraphViz.
module Regulate.NFA.GraphViz where

import Regulate.NFA

import Control.Monad (void)
import Data.Foldable (toList)
import qualified Data.Graph.Inductive.Graph as FGL
import qualified Data.Graph.Inductive.PatriciaTree as FGL
import qualified Data.Graph.Haggle as H
import qualified Data.Graph.Haggle.Internal.Basic as H
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GV
import qualified Data.Text.Lazy as T
import Data.Maybe (fromJust)

edgeLabeledDigraphToFGL :: H.EdgeLabeledGraph H.Digraph el
                        -> FGL.Gr () el
edgeLabeledDigraphToFGL g =
  FGL.mkGraph
  [ (H.vertexId v, ()) | v <- H.vertices g ]
  [ ( H.vertexId (H.edgeSource e)
    , H.vertexId (H.edgeDest e)
    , fromJust (H.edgeLabel g e) )
  | e <- H.edges g ]

graphNFA :: NFA String -> IO ()
graphNFA nfa = do
  let g = edgeLabeledDigraphToFGL (graph nfa)
      params = GV.nonClusteredParams
        { GV.fmtEdge = \(_, _, l) ->
            [GV.Label (GV.StrLabel (T.pack l))]
        , GV.fmtNode = \(n, _) ->
            let fillColor = if n == H.vertexId (startState nfa)
                  then GV.DeepSkyBlue
                  else GV.LightGray
                (penWidth, penColor) =
                  if n `elem` (H.vertexId <$> toList (finalStates nfa))
                  then (3.0, GV.Green)
                  else (1.0, GV.Black)
            in [ GV.style GV.filled
               , GV.fillColor fillColor
               , GV.penWidth penWidth
               , GV.penColor penColor
               ]
        }
      dot = GV.graphToDot params g

  void $ GV.runGraphviz dot GV.Pdf "graph.pdf"

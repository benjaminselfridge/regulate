{-# LANGUAGE TupleSections #-}

-- | Module for rendering NFAs via GraphViz.
module Regulate.NFA.GraphViz where

import Regulate.NFA.Haggle

import Control.Monad (void)
import Data.Foldable (toList)
import qualified Data.Graph.Haggle as H
import qualified Data.Graph.Haggle.Internal.Basic as H
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GV
import qualified Data.Text.Lazy as T

graphNFA :: NFA String String -> IO ()
graphNFA nfa = do
  let verticesWithLabels = [ (v, l) | v <- H.vertices (graph nfa)
                                    , let Just l = H.vertexLabel (graph nfa) v ]
      edgesWithLabels =
        [ (H.edgeSource e, H.edgeDest e, l)
        | e <- H.edges (graph nfa)
        , let Just l = H.edgeLabel (graph nfa) e
        ]
      params = GV.nonClusteredParams
        { GV.fmtEdge = \(_, _, l) ->
            [GV.Label (GV.StrLabel (T.pack $ " " ++ l ++ " "))]
        , GV.fmtNode = \(n, l) ->
            let fillColor = if n == startState nfa
                  then GV.DeepSkyBlue
                  else GV.LightGray
                (penWidth, penColor) =
                  if n `elem` toList (finalStates nfa)
                  then (3.0, GV.Green)
                  else (1.0, GV.Black)
            in [ GV.style GV.filled
               , GV.fillColor fillColor
               , GV.penWidth penWidth
               , GV.penColor penColor
               , GV.toLabel l
               ]
        }
      dot = GV.graphElemsToDot params verticesWithLabels edgesWithLabels

  void $ GV.runGraphviz (H.vertexId <$> dot) GV.Png "graph.png"

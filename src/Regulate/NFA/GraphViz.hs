{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- | Module for rendering NFAs via GraphViz.
module Regulate.NFA.GraphViz where

import Regulate.NFA

import Control.Monad (void)
import Data.Char (toLower)
import Data.Char.Small (toSub)
import Data.Foldable (toList)
import qualified Data.Graph.Haggle as H
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GV
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as T

stateLabel :: Int -> String
stateLabel i = 'q' : (fromJust . toSub <$> show i)

class SymbolLabel s where
  symbolLabel :: s -> String

instance SymbolLabel Char where
  symbolLabel = (:[])

instance SymbolLabel String where
  symbolLabel = id

-- | for creating SymbolLabel instances
eventLabel :: Show e => e -> String
eventLabel = map (toLower . space) . show
  where space '_' = ' '
        space c = c

graphNFA :: SymbolLabel sigma => NFA sigma -> IO ()
graphNFA nfa = do
  let edgesWithLabels =
        [ (H.edgeSource e, H.edgeDest e, l)
        | e <- H.edges (graph nfa)
        , let Just l = H.edgeLabel (graph nfa) e
        ]
      params = GV.nonClusteredParams
        { GV.fmtEdge = \(_, _, l) ->
            [GV.Label (GV.StrLabel (T.pack $ " " ++ symbolLabel l ++ " "))]
        , GV.fmtNode = \(n, _l) ->
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
               , GV.toLabel (stateLabel (H.vertexId n))
               ]
        }
      dot = GV.graphElemsToDot params [(v,v) | v <- H.vertices (graph nfa)] edgesWithLabels

  void $ GV.runGraphviz (H.vertexId <$> dot) GV.Png "graph.png"

graphMNFA :: (SymbolLabel sigma, Ord sigma) => (forall s . MNFA s sigma) -> IO ()
graphMNFA g = graphNFA (buildNFA g)

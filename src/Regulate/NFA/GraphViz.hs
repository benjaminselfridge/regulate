{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- | Module for rendering NFAs via GraphViz.
module Regulate.NFA.GraphViz
  ( graphNFA
  , graphMNFA
  , SymbolLabel(..)
  , eventLabel
  ) where

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

-- | Class for mapping transition symbols to graphviz edge labels. Implement
-- this if you want to graph an NFA with custom events (i.e. anything besides
-- 'Char' or 'String').
class SymbolLabel s where
  symbolLabel :: s -> String

instance SymbolLabel Char where
  symbolLabel = (:[])

instance SymbolLabel String where
  symbolLabel = id

-- | Shows an object, converts all characters to lowercase, and replaces
-- underscores with spaces. This can be handy for writing 'SymbolLabel'
-- instances.
eventLabel :: Show e => e -> String
eventLabel = map (toLower . space) . show
  where space '_' = ' '
        space c = c

-- | Graph an 'NFA' and write it to a PNG file.
graphNFA :: SymbolLabel sigma => FilePath -> NFA sigma -> IO ()
graphNFA path nfa = do
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

  void $ GV.runGraphviz (H.vertexId <$> dot) GV.Png path

-- | Build and graph an 'MNFA' and write it to a PNG file.
graphMNFA :: (SymbolLabel sigma, Ord sigma)
          => FilePath -> (forall s . MNFA s sigma) -> IO ()
graphMNFA path g = graphNFA path (buildNFA g)

{-# LANGUAGE FlexibleInstances #-}
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
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as T

class StateLabel s where
  stateLabel :: s -> String

subInt :: Int -> [Char]
subInt i = fromJust . toSub <$> show i

instance StateLabel Int where
  stateLabel i = 'q' : subInt i

instance StateLabel [Int] where
  stateLabel is = show' $ ('q':) . subInt <$> is
    where show' s = '(' : intercalate "," s ++ ")"

instance StateLabel SimpleLabel where
  stateLabel (IntLabel i) = stateLabel i
  stateLabel (ListLabel is) = stateLabel is

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

graphNFA :: (StateLabel q, SymbolLabel sigma)
         => NFA q sigma -> IO ()
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
            [GV.Label (GV.StrLabel (T.pack $ " " ++ symbolLabel l ++ " "))]
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
               , GV.toLabel (stateLabel l)
               ]
        }
      dot = GV.graphElemsToDot params verticesWithLabels edgesWithLabels

  void $ GV.runGraphviz (H.vertexId <$> dot) GV.Png "graph.png"

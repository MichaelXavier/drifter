module Drifter.Graph
    ( -- * Dependency resolution
      resolveDependencyOrder
    -- * Utilities
    , changesSequence
    ) where

import           Control.Applicative
import           Data.Graph.Inductive (Edge, Gr, UEdge, mkGraph, topsort')
import           Data.List            (foldl')
import qualified Data.Map.Strict      as Map
import           Data.Maybe

import           Drifter.Types

labUEdges :: [Edge] -> [UEdge]
labUEdges = map (\(a, b) -> (a, b, ()))


-- | Sort changes by their dependencies
resolveDependencyOrder :: [Change a] -> [Change a]
resolveDependencyOrder cs = topsort' $ graphDependencies cs

graphDependencies :: [Change a] -> Gr (Change a) ()
graphDependencies cs = mkGraph nodes (labUEdges edges)
    where nodes = zip [1..] cs
          nMap  = Map.fromList $ map (\(i, c) -> (changeName c, i)) nodes
          edges = catMaybes
                $ map (\(a, b) -> (,) <$> a <*> b)
                $ concat
                $ map (\c -> map (\dn -> ( Map.lookup dn nMap
                                         , Map.lookup (changeName c) nMap))
                                 (changeDependencies c))
                      cs

-- | Treat a list of changes like a sequence where each subsequent
-- 'Change' depends on only its predecessor. Note that this overwrites
-- the dependencies attribute of each change.
changesSequence :: [Change a] -> [Change a]
changesSequence [] = []
changesSequence (x:xs) = reverse $ snd $ foldl' go (x, [x]) xs
  where go :: (Change a, [Change a]) -> Change a -> (Change a, [Change a])
        go (lastChange, xs') c =
          let c' = c { changeDependencies = [changeName lastChange] }
          in (c', c':xs')


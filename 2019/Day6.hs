{- stack script
    --resolver lts-14.12
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Array ((!))
import qualified Data.Array as Array
import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- mkGraph . map (fmap tail . break (== ')')) . lines <$> readFile "Day6.txt"

  print $ part1 input
  print $ part2 input

part1 :: Graph -> Int
part1 Graph{graph} = sum . map (subtract 1 . length . Graph.reachable graph) . Array.indices $ graph

part2 :: Graph -> Int
part2 Graph{graph, getVertex} = minimum $ Map.intersectionWith (+) reachableFromYOU reachableFromSAN
  where
    -- get the planet the given person is orbitting around
    getPlanet person = case (graph !) <$> getVertex person of
      Just [planet] -> planet
      _ -> error $ person ++ " is not in graph or is not orbitting exactly one planet"
    vertexYOU = getPlanet "YOU"
    vertexSAN = getPlanet "SAN"

    reachableFromYOU = reachableWithDistance vertexYOU
    reachableFromSAN = reachableWithDistance vertexSAN

    reachableWithDistance start =
      let go !i (Graph.Node curr rest) = (curr, i) : concatMap (go (i + 1)) rest
      in Map.fromList $ go 0 $ head $ Graph.dfs graph [start]

{- Graph -}

-- A wrapper around Graph.Graph to keep track of Graph state.
data Graph = Graph
  { graph     :: Graph.Graph
  , getVertex :: String -> Maybe Graph.Vertex
  }

mkGraph :: [(String, String)] -> Graph
mkGraph = fst3 . Graph.graphFromEdges . map (\(src, dests) -> (src, src, dests)) . mkAdjList
  where
    mkAdjList = Map.toList . Map.fromListWith (++) . concatMap
      (\(dest, src) ->
        [ (src, [dest])
        -- need this in case of nodes without any outgoing edges, since graphFromEdges
        -- ignores keys in the outlist that don't correspond to nodes in the graph
        , (dest, [])
        ]
      )
    fst3 (graph, _, getVertex) = Graph{..}

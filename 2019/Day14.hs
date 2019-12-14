{- stack script
    --resolver lts-14.12
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE TypeApplications #-}

import qualified Data.Graph as Graph
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)
import Data.Tuple.Extra (fst3)

main :: IO ()
main = do
  input <- readReactions <$> readFile "Day14.txt"

  print $ part1 input

part1 :: Reactions -> Int
part1 reactions = (! "ORE") . foldr resolve initialRequirements . tail . getReactionOrder $ reactions
  where
    initialRequirements = Map.insert "ORE" 0 . Map.insert "FUEL" 1 . Map.map (const 0) $ reactions

    resolve resource requirements =
      let requirement = requirements ! resource
          (quantityPerReaction, equations) = reactions ! resource
          numReactions = ceiling (fromIntegral requirement / fromIntegral quantityPerReaction :: Double)
          addToRequirements (inputResource, quantity) = Map.adjust (+ (numReactions * quantity)) inputResource
      in foldr addToRequirements requirements equations

type Resource = String
type Quantity = Int
type Equation = (Resource, Quantity)

type Reactions = Map Resource (Quantity, [Equation])

readReactions :: String -> Reactions
readReactions = Map.fromList . map parseReaction . lines
  where
    parseReaction line = case splitOn " => " line of
      [lhs, rhs] ->
        let (resource, quantity) = parseResource rhs
            equations = map parseResource $ splitOn ", " lhs
        in (resource, (quantity, equations))
      _ -> error $ "Invalid line: " ++ line

    parseResource resource = case words resource of
      [count, name] -> (name, read @Int count)
      _ -> error $ "Invalid resource: " ++ resource

-- | Get the topological sort of resources in the given list of reactions
--
-- ORE => A; A => B; B => FUEL
--
-- would result in [ORE, A, B, FUEL]
getReactionOrder :: Reactions -> [Resource]
getReactionOrder outputToInputs = map (fst3 . fromVertex) . Graph.topSort $ graph
  where
    (graph, fromVertex, _) = Graph.graphFromEdges $ map dupFirst $ ("FUEL", []) : Map.toList inputToOutputs
    dupFirst (a, b) = (a, a, b)

    inputToOutputs = Map.fromListWith (++)
        [ (input, [output])
        | (output, (_, inputs)) <- Map.toList outputToInputs
        , (input, _) <- inputs
        ]

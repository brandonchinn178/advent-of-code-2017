{- stack script
    --resolver lts-12.12
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (ord)
import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import qualified Data.Text as Text

type Graph = Map Char (Set Char)
data Simulation = Simulation
  { graph :: Graph
  , dependsOn :: Graph
  , now :: Int
  , todo :: Set Char
  , inProgress :: [(Int, Char)]
  , result :: [Char]
  } deriving (Show)

main :: IO ()
main = do
  input <- mkSimulation . map parse . lines <$> readFile "Day7.txt"
  let example = mkSimulation [('C', 'A'), ('C', 'F'), ('A', 'B'), ('A', 'D'), ('B', 'E'), ('D', 'E'), ('F', 'E')]
  print $ part1 example
  print $ part1 input
  print $ part2 (2, 0) example
  print $ part2 (5, 60) input

mkSimulation :: [(Char, Char)] -> Simulation
mkSimulation l = Simulation{..}
  where
    minNode = minimum $ map (uncurry min) l
    maxNode = maximum $ map (uncurry max) l
    graph = mkGraph l
    dependsOn = mkGraph $ map (uncurry $ flip (,)) l
    hasDepends = Map.keysSet dependsOn
    now = 0
    todo = Set.fromList $ filter (`Set.notMember` hasDepends) [minNode..maxNode]
    inProgress = []
    result = []

mkGraph :: [(Char, Char)] -> Graph
mkGraph = Map.fromListWith (<>) . map (\(x, y) -> (x, Set.singleton y))

step :: Simulation -> (Bool, Simulation)
step sim@Simulation{..} =
  case filter notBlocked (Set.toAscList todo) of
    [] -> (True, sim)
    curr:_ -> (,) False $ sim
      { todo = Set.delete curr todo
      , inProgress = (now, curr) : inProgress
      }
  where
    notBlocked = Set.null . (\\ Set.fromList result) . from dependsOn

isDone :: Simulation -> Bool
isDone Simulation{..} = null todo && null inProgress

complete :: [Char] -> Simulation -> Simulation
complete cs sim@Simulation{..} = sim
  { todo = todo <> (new \\ Set.fromList result)
  , result = cs ++ result
  }
  where
    new = mconcat $ map (from graph) cs

part1 :: Simulation -> [Char]
part1 = check . iterate (post . step)
  where
    check (curr:rest) = if isDone curr
      then reverse $ result curr
      else check rest
    post (_, sim@Simulation{..}) =
      let curr = map snd inProgress
      in complete curr $ sim { inProgress = [] }

part2 :: (Int, Int) -> Simulation -> Int
part2 (maxWorkers, delay) = check . iterate (post . step)
  where
    getEnd (start, c) = start + (ord c - 64 + delay)
    check (curr@Simulation{..}:rest) = if isDone curr
      then now
      else check rest
    post (blocked, sim@Simulation{..})
      | not (null todo) && length inProgress < maxWorkers && not blocked = sim
      | otherwise =
          let nextNow = minimum $ map getEnd inProgress
              (finished, nextInProgress) = partition ((== nextNow) . getEnd) inProgress
          in complete (map snd finished) $ sim
            { now = nextNow
            , inProgress = nextInProgress
            }

parse :: String -> (Char, Char)
parse = fromLine . map Text.unpack . Text.splitOn "tep " . Text.pack
  where
    fromLine = \case
      [_, x:_, y:_] -> (x, y)
      l -> error $ "Invalid: " ++ show l

from :: Graph -> Char -> Set Char
from graph x = Map.findWithDefault Set.empty x graph

flatten :: [(a, a)] -> [a]
flatten = concat . map (\(x, y) -> [x, y])

first :: (a -> Bool) -> [a] -> a
first _ [] = error "No first"
first f (x:xs) = if f x then x else first f xs

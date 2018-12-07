{- stack script
    --resolver lts-12.12
-}
{-# LANGUAGE MultiWayIf #-}

import Data.Function (on)
import Data.List (maximumBy, sort, sortBy)
import Data.List.Split (splitOn, splitOneOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace

type GuardId = Int
type Interval = (Int, Int)
type Log = Map GuardId [Interval]

main :: IO ()
main = do
  input <- parse . sort . lines <$> readFile "Day4.txt"
  print $ part1 input
  print $ part2 input

parse :: [String] -> Log
parse = parse' Map.empty uninit uninit
  where
    uninit = error "empty"
    parse' acc _ _ [] = acc
    parse' acc curr start (x:xs) =
      let [time, text] = splitOn "] " x
          minute = read $ last $ splitOn ":" time
      in if
        | [_, t] <- splitOn "Guard #" text
          -> let guardId = read $ head $ splitOn " " t
             in parse' (Map.insertWith (++) guardId [] acc) guardId uninit xs
        | text == "falls asleep" -> parse' acc curr minute xs
        | text == "wakes up"
          -> parse' (Map.adjust ((start, minute):) curr acc) curr uninit xs
        | otherwise -> error $ "Invalid text: " ++ text

part1 :: Log -> Int
part1 input = mostGuard * findMostMinute (input Map.! mostGuard)
  where
    diffs = Map.map (sum . map (uncurry (flip (-)))) input
    most = maxMap diffs
    mostGuard = fst most
    findMostMinute times = fst
      $ maximumBy (compare `on` snd)
      $ map (\i -> (i, count (contains i) times)) [0..59]

part2 :: Log -> Int
part2 input = maxGuard * maxMinute
  where
    minutes = [(i, maxMap $ Map.map (count (contains i)) input) | i <- [0..59]]
    (maxMinute, (maxGuard, _)) = maximumBy (compare `on` (snd . snd)) minutes

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

contains :: Int -> Interval -> Bool
contains i (start, end) = i >= start && i < end

maxMap :: Ord v => Map k v -> (k, v)
maxMap = head . sortMap
  where
    sortMap = reverse . sortBy (compare `on` snd) . Map.toList

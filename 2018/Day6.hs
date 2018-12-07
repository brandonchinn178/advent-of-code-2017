{- stack script
    --resolver lts-12.12
-}

{-# LANGUAGE RecordWildCards #-}

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

data Input = Input
  { coords :: [(Int, Int)]
  , maxX :: Int
  , maxY :: Int
  , board :: [(Int, Int)]
  }

main :: IO ()
main = do
  coords <- map parse . lines <$> readFile "Day6.txt"
  let maxX = maximum (map fst coords) - 1
      maxY = maximum (map snd coords) - 1
      board = [(x, y) | x <- [0..maxX], y <- [0..maxY]]
      input = Input{..}
  print $ part1 input
  print $ part2 input

parse :: String -> (Int, Int)
parse s = case splitOn ", " s of
  [x, y] -> (read x, read y)
  _ -> error "Bad input"

part1 :: Input -> Int
part1 Input{..} = maximum $ map (count finiteCoords) coords
  where
    border = filter (\(x, y) -> x == 0 || y == 0 || x == maxX || y == maxY) board
    getClosestTo spot = keepMinimumBy (distance spot) coords
    labelledCoords = flip mapMaybe board $ \spot ->
      case getClosestTo spot of
        [coord] -> Just (spot, coord)
        _ -> Nothing
    infiniteCoords = map snd $ filter ((`elem` border) . fst) labelledCoords
    finiteCoords = filter (`notElem` infiniteCoords) $ map snd labelledCoords

part2 :: Input -> Int
part2 Input{..} = length $ filter (< 10000) $ map check board
  where
    check spot = sum $ map (distance spot) coords

{- Utils -}

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

keepMinimumBy :: Ord b => (a -> b) -> [a] -> [a]
keepMinimumBy f l = map fst $ filter ((== lowest) . snd) l'
  where
    l' = map (id &&& f) l
    lowest = minimum $ map snd l'

count :: Eq a => [a] -> a -> Int
count xs x = length $ filter (== x) xs

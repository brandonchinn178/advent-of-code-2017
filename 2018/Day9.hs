{- stack script
    --resolver lts-12.12
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Data.List (groupBy)
import qualified Data.Map.Strict as Map

data Circle = Circle
  { prev  :: [Int]
  , next  :: [Int]
  } deriving (Show)

main :: IO ()
main = do
  let numPlayers = 471
      lastMarble = 72026
  print $ part1 numPlayers lastMarble
  print $ part2 numPlayers lastMarble

part1 :: Int -> Int -> Int
part1 numPlayers lastMarble = maximum $ Map.fromListWith (+) playerToScore
  where
    (_, scores) = run lastMarble
    playerToScore = zip (map ((`mod` numPlayers) . (* 23)) [1..]) scores

run :: Int -> (Circle, [Int])
run lastMarble = foldl place (start, []) sections
  where
    start = Circle [] [0]
    sections = groupBy (\x y -> not (div23 x || div23 y)) [1..lastMarble]
    div23 x = x `mod` 23 == 0
    place (curr, scores) = \case
      [num] | div23 num ->
        let circle@Circle{..} = rewind 7 curr
        in case next of
          [] -> error "unreachable"
          node:rest -> (circle{next = rest}, (node + num) : scores)
      todo -> (placeAll curr todo True, scores)

placeAll :: Circle -> [Int] -> Bool -> Circle
placeAll circle [] _ = circle
placeAll circle@Circle{..} todo@(t:ts) skip =
  case next of
    [] -> placeAll (Circle [] $ reverse prev) todo skip
    node:rest ->
      let curr = circle{prev = node:prev}
      in if skip
        then placeAll curr{next = rest} todo False
        else placeAll curr{next = t:rest} ts True

rewind :: Int -> Circle -> Circle
rewind 0 circle = circle
rewind i circle@Circle{..} = case prev of
  [] -> rewind i $ Circle (reverse next) []
  (node:rest) -> rewind (i - 1) $ Circle rest (node:next)

part2 :: Int -> Int -> Int
part2 numPlayers lastMarble = part1 numPlayers $ lastMarble * 100

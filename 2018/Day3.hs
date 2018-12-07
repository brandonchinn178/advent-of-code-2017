{- stack script
    --resolver lts-12.12
-}

{-# LANGUAGE RecordWildCards #-}

import Data.List.Split (splitOneOf)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- map toClaim . lines <$> readFile "Day3.txt"
  print $ part1 input
  print $ part2 input

data Claim = Claim
  { claimId :: Int
  , x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  } deriving (Show)

toClaim :: String -> Claim
toClaim = toClaim' . map read . filter (not . null) . splitOneOf "#@,:x "
  where
    toClaim' [claimId, x, y, width, height] = Claim{..}

part1 :: [Claim] -> Int
part1 = Set.size . getOverlap

part2 :: [Claim] -> Claim
part2 claims = head $ filter (Set.disjoint overlap . toArea) claims
  where
    overlap = getOverlap claims

toArea :: Claim -> Set (Int, Int)
toArea Claim{..} = Set.fromList
  [ (x + dx, y + dy)
  | dx <- [0..width - 1]
  , dy <- [0..height - 1]
  ]

getOverlap :: [Claim] -> Set (Int, Int)
getOverlap = snd . foldl track (Set.empty, Set.empty) . map toArea
  where
    track (seen, common) set = (seen <> set, common <> Set.intersection seen set)

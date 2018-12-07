{- stack script
    --resolver lts-12.12
-}

{-# LANGUAGE TupleSections #-}

import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- lines <$> readFile "Day2.txt"
  print $ part1 input
  putStrLn $ part2 input

part1 :: [String] -> Int
part1 = uncurry (*) . (count (has 2) &&& count (has 3))
  where
    has x = any (== x) . Map.elems . foldr (Map.alter plus1) Map.empty
    plus1 = Just . maybe 1 (+ 1)
    count f = length . filter id . map f

part2 :: [String] -> String
part2 = map fst . compare (==) . fst . get ((== 1) . snd) . map (id &&& diff) . pairs
  where
    pairs (x:xs) = map (x,) xs ++ pairs xs
    pairs [] = []
    diff = length . compare (/=)
    compare f = filter (uncurry f) . uncurry zip
    get f = head . filter f

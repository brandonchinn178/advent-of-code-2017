{- stack script
    --resolver lts-12.12
    --package containers
-}

import Data.Bifunctor (bimap)
import Data.List (uncons)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- map readInt . lines <$> readFile "Day1.txt"
  print $ part1 input
  print $ part2 input

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = part2' 0 Set.empty . cycle
  where
    part2' curr seen (x:xs) =
      if curr `Set.member` seen
        then curr
        else part2' (curr + x) (Set.insert curr seen) xs
    part2' _ _ [] = error "Empty list passed to part2"

readInt :: String -> Int
readInt = uncurry (*) . bimap sign read . fromJust . uncons
  where
    sign '+' = 1
    sign '-' = -1
    sign _ = error "Invalid sign"

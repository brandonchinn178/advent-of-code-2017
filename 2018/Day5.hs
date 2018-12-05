{- stack script
    --resolver lts-12.12
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.Function (on)
import Data.List (minimumBy)
import qualified Data.Text as Text

main :: IO ()
main = do
  input <- readFile "Day5.txt"
  print $ part1 input
  print $ part2 input

reacts :: Char -> Char -> Bool
reacts x y = lowerX == lowerY && one isLower && one isUpper
  where
    lowerX = toLower x
    lowerY = toLower y
    one f = length (filter f [x, y]) == 1

reduce :: String -> String
reduce = \case
  x:y:xs -> loop [] x y xs
  s -> s
  where
    loop start x y end =
      if reacts x y
        then case (start, end) of
          ([], _) -> reduce end
          (_, []) -> reverse start
          (s:ss, e:es) -> loop ss s e es
        else case end of
          [] -> reverse $ y:x:start
          (e:es) -> loop (x:start) y e es

part1 :: String -> Int
part1 = length . reduce

part2 :: String -> Int
part2 s = length $ minimumBy (compare `on` length) $ map (reduce . remove) ['a'..'z']
  where
    remove c =
      Text.unpack
      . Text.replace (Text.singleton $ toUpper c) ""
      . Text.replace (Text.singleton c) ""
      . Text.pack
      $ s

module Main where

import Utils

import Data.List (permutations)

getPuzzle :: IO [[String]]
getPuzzle = (map words . lines) <$> readFile "day4.txt"

-- | Check that all pairs of words in the given phrase satisfy
-- the given predicate.
comparePairs :: (String -> String -> Bool) -> [String] -> Bool
comparePairs pred = all (uncurry pred) . allPairs

noDuplicates :: [String] -> Bool
noDuplicates = comparePairs (/=)

isAnagram :: String -> String -> Bool
isAnagram a b = b `elem` permutations a

noAnagrams :: [String] -> Bool
noAnagrams = comparePairs isAnagram'
  where
    isAnagram' a = not . isAnagram a

count :: ([String] -> Bool) -> [[String]] -> Int
count pred = length . filter pred

main :: IO ()
main = do
  part 1 $ do
    check "noDuplicates" noDuplicates
      [ (["aa", "bb", "cc", "dd", "ee"], True)
      , (["aa", "bb", "cc", "dd", "aa"], False)
      , (["aa", "bb", "cc", "dd", "aaa"], True)
      ]
    check "count" (count noDuplicates)
      [ ([["a", "b"], ["a", "a"], ["c", "a"]], 2)
      ]
    getPuzzle >>= solve . count noDuplicates

  part 2 $ do
    check "noAnagrams" noAnagrams
      [ (["a", "a"], False)
      , (["ab", "ba"], False)
      , (["ac", "cb"], True)
      , (["abcde", "fghij"], True)
      , (["abcde", "xyz", "ecdab"], False)
      , (["a", "ab", "abc", "abd", "abf", "abj"], True)
      , (["iiii", "oiii", "ooii", "oooi", "oooo"], True)
      , (["oiii", "ioii", "iioi", "iiio"], False)
      ]
    getPuzzle >>= solve . count noAnagrams

{-# LANGUAGE TupleSections #-}

module Utils where

import Control.Monad (when)
import Data.List (tails)

-- Helpers to check/solve in main

part :: Int -> IO () -> IO ()
part partNum action = do
  putStrLn $ "\n========== Part " ++ show partNum ++ " =========="
  action

check :: (Show a, Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> IO ()
check label f cases = do
  putStrLn $ "\nTesting " ++ label ++ ":"
  mapM_ checkCase cases
  where
    checkCase (input, expected) = do
      putStrLn $ "- " ++ show input ++ " -> " ++ show expected
      let result = f input
      when (expected /= result) $ fail $ "Check failed: got " ++ show result

check' :: String -> [Bool] -> IO ()
check' label = assert label . map (, True)

assert :: (Show a, Eq a) => String -> [(a, a)] -> IO ()
assert label = check label id

solve :: Show a => a -> IO ()
solve result = do
  putStrLn "\nSolving puzzle:"
  putStrLn $ "- Result: " ++ show result

-- Utility functions

-- | Get all pairs of items in the list.
allPairs :: [a] -> [(a, a)]
allPairs l = [(a, b) | (a:bs) <- tails l, b <- bs]

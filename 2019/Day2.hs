{- stack script
    --resolver lts-14.12
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector

type Program = Vector Int

main :: IO ()
main = do
  input <- Vector.fromList . map (read @Int . Text.unpack) . Text.splitOn "," . Text.pack <$> readFile "Day2.txt"
  print $ part1 input
  print $ part2 input

part1 :: Program -> Int
part1 = runIntCode 12 2

part2 :: Program -> (Int, Int)
part2 prog = go [(noun, verb) | noun <- [0..99], verb <- [0..99]]
  where
    go [] = error "no valid answer"
    go ((noun, verb) : rest) =
      if runIntCode noun verb prog == 19690720
        then (noun, verb)
        else go rest

runIntCode :: Int -> Int -> Program -> Int
runIntCode noun verb = Vector.head . runProgram . setNounVerb
  where
    setNounVerb = (// [(1, noun), (2, verb)])

runProgram :: Program -> Program
runProgram = go 0
  where
    go pos prog = case prog ! pos of
      1 ->
        let [x, y, res] = Vector.toList $ Vector.slice (pos + 1) 3 prog
        in go (pos + 4) $ prog // [(res, prog ! x + prog ! y)]
      2 ->
        let [x, y, res] = Vector.toList $ Vector.slice (pos + 1) 3 prog
        in go (pos + 4) $ prog // [(res, prog ! x * prog ! y)]
      99 -> prog
      x -> error $ "bad input: " ++ show x

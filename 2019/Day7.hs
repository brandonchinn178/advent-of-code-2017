{- stack script
    --resolver lts-14.12
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap
import Data.List (maximumBy, permutations, uncons)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  program <- readProgram <$> readFile "Day7.txt"

  print $ part1 program
  print $ part2 program

part1 :: Program -> (Int, PhaseSettings)
part1 = maximumBy (compare `on` fst) . runAllPhaseSettings (0, 4)

part2 :: Program -> (Int, PhaseSettings)
part2 = maximumBy (compare `on` fst) . runAllPhaseSettings (5, 9)

{- PhaseSettings -}

type PhaseSettings = [Int]

runAllPhaseSettings :: (Int, Int) -> Program -> [(Int, PhaseSettings)]
runAllPhaseSettings (start, end) program = map (runPhaseSettings program &&& id) allPhaseSettings
  where
    allPhaseSettings = permutations [start..end]

runPhaseSettings :: Program -> [Int] -> Int
runPhaseSettings program phaseSettings = last outputsFromLastAmp
  where
    outputsFromLastAmp = last allOutputs
    allOutputs = runAmps (0 : outputsFromLastAmp) phaseSettings

    -- reimplementing scanl for readability
    runAmps _ [] = []
    runAmps inputs (phaseSetting : rest) =
      let outputs = runProgram program $ phaseSetting : inputs
      in outputs : runAmps outputs rest

{- Program -}

type Program = IntMap Int

data ProgramState = ProgramState
  { program  :: Program
  , position :: Address
  , inputs   :: [Int]
  } deriving (Show)

readProgram :: String -> Program
readProgram = IntMap.fromList . zip [0..] . map (read @Int) . splitOn ","

runProgram :: Program -> [Int] -> [Int]
runProgram program inputs = execProgram ProgramState { position = 0, .. }

execProgram :: ProgramState -> [Int]
execProgram state =
  case getCommand (program state) (position state) of
    ADD p1 p2 out -> execBinOp (+) p1 p2 out
    MULTIPLY p1 p2 out -> execBinOp (*) p1 p2 out
    INPUT out ->
      let (input, rest) = nextInput
      in execProgram . bumpPosition 2 . updateProgram out input $ state { inputs = rest }
    OUTPUT p1 -> resolve p1 : (execProgram . bumpPosition 2 $ state)
    JUMP_TRUE p1 p2 -> execJump (/= 0) p1 p2
    JUMP_FALSE p1 p2 -> execJump (== 0) p1 p2
    COMPARE_LT p1 p2 out -> execComp (<) p1 p2 out
    COMPARE_EQ p1 p2 out -> execComp (==) p1 p2 out
    HALT -> []
  where
    nextInput =
      let ProgramState{..} = state
          errorMessage = "Ran out of inputs for the INPUT command at position: " ++ show position
      in fromMaybe (error errorMessage) $ uncons inputs

    -- executors
    execBinOp f p1 p2 out =
      let result = resolveBinOp f p1 p2
      in execProgram . bumpPosition 4 . updateProgram out result $ state
    execJump f p1 p2 =
      let updatePosition = maybe (bumpPosition 3) setPosition $ resolveJump f p1 p2
      in execProgram . updatePosition $ state
    execComp f p1 p2 out =
      let result = resolveComp f p1 p2
      in execProgram . bumpPosition 4 . updateProgram out result $ state

    -- resolvers
    resolve = resolveParam (program state)
    resolveBinOp f p1 p2 = resolve p1 `f` resolve p2
    resolveJump f p1 p2 = if f (resolve p1) then Just (resolve p2) else Nothing
    resolveComp f p1 p2 = if resolve p1 `f` resolve p2 then 1 else 0

    -- state updaters
    setPosition pos s = s { position = pos }
    bumpPosition n s = s { position = position s + n }
    updateProgram out result s = s { program = IntMap.insert out result (program s) }

{- Parameters -}

type Address = Int
type Value = Int

data Parameter
  = ADDRESS Address
  | VALUE Value
  deriving (Show)

resolveParam :: Program -> Parameter -> Value
resolveParam program = \case
  ADDRESS address -> program ! address
  VALUE value -> value

{- Commands -}

data Command
  = ADD Parameter Parameter Address
  | MULTIPLY Parameter Parameter Address
  | INPUT Address
  | OUTPUT Parameter
  | JUMP_TRUE Parameter Parameter
  | JUMP_FALSE Parameter Parameter
  | COMPARE_LT Parameter Parameter Address
  | COMPARE_EQ Parameter Parameter Address
  | HALT
  deriving (Show)

getCommand :: Program -> Address -> Command
getCommand program position =
  case opcode of
    1 -> buildBinOp ADD
    2 -> buildBinOp MULTIPLY
    3 -> INPUT (rawParameter 0)
    4 -> OUTPUT (parseParameter 0)
    5 -> buildJump JUMP_TRUE
    6 -> buildJump JUMP_FALSE
    7 -> buildComp COMPARE_LT
    8 -> buildComp COMPARE_EQ
    99 -> HALT
    _ -> error $ "Invalid opcode: " ++ show opcode
  where
    (modes, opcode) = (program ! position) `divMod` 100

    rawParameter digit = program ! (position + digit + 1)
    parseParameter digit =
      let parameter = rawParameter digit
      in case getDigit digit modes of
        0 -> ADDRESS parameter
        1 -> VALUE parameter
        mode -> error $ "Invalid parameter mode: " ++ show mode

    buildBinOp f = f (parseParameter 0) (parseParameter 1) (rawParameter 2)
    buildJump f = f (parseParameter 0) (parseParameter 1)
    buildComp f = f (parseParameter 0) (parseParameter 1) (rawParameter 2)

{- Utilities -}

-- | Get the given digit from the number, starting from the least significant
-- digit.
getDigit :: Int -> Int -> Int
getDigit digit x = (x `div` 10 ^ digit) `mod` 10

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _ [] = error "Cannot call mapFirst on an empty list"
mapFirst f (x:xs) = f x : xs

-- Move the last element in the list to the front
unshift :: [a] -> [a]
unshift [] = error "unshift called on an empty list"
unshift l = last l : init l

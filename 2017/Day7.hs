{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.List (intercalate, findIndices)

import Utils

data Program = Program
  { name :: String
  , weight :: Int
  , children :: [String]
  , parent :: Maybe String
  } deriving (Eq)

instance Show Program where
  show Program{..} =
    name ++ " (" ++ show weight ++ ") [" ++ intercalate ", " children ++ "]"

parsePuzzle :: String -> Program
parsePuzzle p = Program{..}
  where
    parent = Nothing
    name : parenWeight : rest = words p
    weight = read $ tail $ init $ parenWeight
    children = case rest of
      "->":rest' -> map stripComma rest'
      _ -> []
    stripComma s = if last s == ',' then init s else s

getPuzzle :: IO [Program]
getPuzzle = (map parsePuzzle . lines) <$> readFile "day7.txt"

testPuzzle :: [Program]
testPuzzle = map (\(n,w,c) -> Program n w c Nothing)
  [ ("pbga", 66, [])
  , ("xhth", 57, [])
  , ("ebii", 61, [])
  , ("havc", 66, [])
  , ("ktlj", 57, [])
  , ("fwft", 72, ["ktlj", "cntj", "xhth"])
  , ("qoyq", 66, [])
  , ("padx", 45, ["pbga", "havc", "qoyq"])
  , ("tknk", 41, ["ugml", "padx", "fwft"])
  , ("jptl", 61, [])
  , ("ugml", 68, ["gyxo", "ebii", "jptl"])
  , ("gyxo", 61, [])
  , ("cntj", 57, [])
  ]

getNameMap :: [Program] -> HashMap String Program
getNameMap = populateParents . HM.fromList . map (\p -> (name p, p))
  where
    populateParents mapping = foldl populate mapping $ HM.elems mapping
    -- For a given program, set all its children's parent to itself
    populate m prog = foldl (setParent prog) m $ children prog
    setParent parentProg m progName =
      let
        prog = lookup' progName m
        newProg = prog { parent = Just $ name parentProg }
      in HM.insert progName newProg m

lookup' :: (Eq k, Hashable k) => k -> HashMap k v -> v
lookup' = HM.lookupDefault $ error "Could not find key"

-- | Get the name of then program at the bottom of the tower.
getBottom :: [Program] -> String
getBottom programs = name $ getBottom' programs $ getNameMap programs

-- | Get the program at the bottom of the tower.
getBottom' :: [Program] -> HashMap String Program -> Program
getBottom' programs mapping = getParent first
  where
    getParent p = case parent p of
      Nothing -> p
      Just next -> getParent $ lookup' next mapping
    first = lookup' (name $ head programs) mapping

-- | Get the weight of the given program and its children.
getWeight :: String -> HashMap String Program -> Int
getWeight progName mapping = weight prog + childrenWeight
  where
    prog = lookup' progName mapping
    childrenWeight = sum $ map lookupWeight $ children prog
    lookupWeight c = weight $ lookup' c mapping

-- | Get the program that is the wrong weight and the weight it
-- needs to be.
getWrongProgram :: [Program] -> (Program, Int)
getWrongProgram programs = go 0 $ getBottom' programs mapping
  where
    mapping = getNameMap programs
    go weight prog =
      let children' = map (\c -> (getWeight c mapping, c)) $ children prog
      in case minority children' of
        Nothing -> (prog, weight)
        Just (weight', next) -> go weight' $ lookup' next mapping
    minority [(wa, a), (wb, b)]
      | wa == wb = Nothing
      | otherwise = Just (wa, a)
    minority l
      | length l <= 1 = Nothing
      | otherwise =
        let
          weights = map fst l
          maxWeight = maximum weights
          minWeight = minimum weights
          maxIndices = findIndices (== maxWeight) weights
          minIndices = findIndices (== minWeight) weights
          wrongIndex = head $ if length maxIndices == 1
            then maxIndices
            else minIndices
        in if maxWeight == minWeight
          then Nothing
          else Just $ l !! wrongIndex

main :: IO ()
main = do
  part 1 $ do
    check "parsePuzzle" parsePuzzle
      [ ("uwzmqi (57)", Program "uwzmqi" 57 [] Nothing)
      , ( "emlzcpy (106) -> pwmoihf, sdwnkb"
        , Program "emlzcpy" 106 ["pwmoihf", "sdwnkb"] Nothing
        )
      ]

    let test = [Program "a" 0 ["b"] Nothing, Program "b" 0 [] Nothing]
    let testResult1 = getNameMap test
    let testResult2 = getNameMap $ reverse test
    check' "populateParents"
      [ parent (lookup' "a" testResult1) == Nothing
      , parent (lookup' "b" testResult1) == Just "a"
      , parent (lookup' "a" testResult2) == Nothing
      , parent (lookup' "b" testResult2) == Just "a"
      ]

    check "getBottom" getBottom
      [ (test, "a")
      , (reverse test, "a")
      ]

    getPuzzle >>= solve . getBottom

  part 2 $ do
    let a = Program "a" 10 ["b"] Nothing
    let b = Program "b" 5 [] Nothing
    let c = Program "c" 15 [] Nothing
    let mapping = getNameMap [a, b, c]
    assert "getWeight"
      [ (getWeight "a" mapping, 15)
      , (getWeight "b" mapping, 5)
      , (getWeight "c" mapping, 15)
      ]

    let testWrong = getWrongProgram testPuzzle
    check' "getWrongProgram"
      [ name (fst testWrong) == "ugml"
      , snd testWrong == 60
      ]

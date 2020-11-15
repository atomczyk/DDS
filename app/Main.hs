module Main where

import AuxiliaryTrees
import Language
import AuxiliaryFormulas
import AuxiliaryTrees
import CutRule
import TheClassicalLogicRule
import PrintingTrees
import Generator
import FlexibleTrees
import CanonicalTrees
import Data.String
import Data.List
import Canonical
import Dual
import Data
import DecisionTree
import RandomFor
import System.Random
import System.Random.Shuffle
import Printing

main :: IO()
main = do
    let g = mkStdGen 356
    let fors = take 100 $ randomRs (V 20, V 5) g
    let forss = map Printing.printFor fors
--  print $ head forss
--    writeFile "data/long_formulas.txt" columns
    writeFile "data/100formulas.txt" $ unlines $ forss

{-
  do
  let g = mkStdGen 356
  print $ take 300000 $ randomRs (V 14, V 3) g

  do
  let g = mkStdGen 356
  let t = fst $ randomRs (V 20, V 3) g
  print $ Printing.printFor t
-}
  --do
--  writeFile "decsision_tree_test.txt" columns
--  appendFile "decsision_tree_test.txt" $ unlines $ dataCSV Data.test

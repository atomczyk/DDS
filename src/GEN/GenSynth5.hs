module GenSynth5 where

import AuxiliaryTrees
import Language
--import LanguageMbC
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
import Canonical_KE
import Dual
import Dual2
import Dual3
import Data
import DecisionTree
--import RandomFor
import RandomFor2
import RandomMbC
import System.Random
import System.Random.Shuffle
import Printing
--import PrintingMbC
import Parsing
import System.IO
import Res_Test2
import Data.List.Split

-- Aktualnie analizowane formuły
g = read "357 1" :: StdGen
gen = mkDetGenerator [I Verum Verum, N Verum]
sgen = genSamples gen

-- Generates all possible synthetic tableaux for a formula, each formula has its own file
m :: IO()
m = do
  let (fors, _) = sgen [23] [9..12] [5] 25 g
--  let (fors, _) = sgen [23] [9..12] [2] 25 g
--  let formulas = chunksOf 10 fors
--  let numbers = chunksOf 10 [1..100]
  let pairs = zip [81..101] (take 20 (drop 80 fors))
--  let info = [writeFile ("data/formulas/" ++ "F" ++ show x ++ ".txt") (unlines $ mainFunctionMichal1 y)  | (x, y) <- pairs]
  let info = [writeFile ("data/formulas/" ++ "F" ++ show x ++ ".txt") (Printing.printFor y ++ "\n") >> appendFile ("data/formulas/" ++ "F" ++ show x ++ ".txt") (unlines $ mainFunctionMichal1 y)  | (x, y) <- pairs]
  sequence_ info

-- Generates all possible synthetic tableaux for chosen formulas
michal :: IO()
michal = do
  let (fors, _) = sgen [23] [9..12] [5] 25 g
  let info = concat $ map mainFunctionMichal fors
  let columns = "formula" ++ ", " ++ "good_instruction" ++ ", " ++ "is_taut" ++ ", " ++ "num_of_nodes" ++ ", " ++ "num_of_branches" ++ ", " ++ "b_depth\n"
  writeFile "/media/dds/9e4adeb4-fe12-442e-b638-c1c3c647a2f6/gen[23][9..12][5]25.txt" columns
  appendFile "/media/dds/9e4adeb4-fe12-442e-b638-c1c3c647a2f6/gen[23][9..12][5]25.txt" $ unlines $ info

mainFunctionMichal :: For -> [String]
mainFunctionMichal for = [(Printing.printFor for) ++ ", " ++ showInstructions (exportIndex11 y) ++ ", " ++ show (is_taut)
    ++ ", " ++ show (numberOfNodes (synthFlex y for)) ++ ", " ++ show (numberOfBranches (synthFlex y for)) ++ ", " ++ show (b_depthOfTree (synthFlex y for)) | y <- goodInstructions]
    where
      goodInstructions  = allRules (genBase for)
      is_taut           = Canonical.can_isTaut for

mainFunctionMichal1 :: For -> [String]
mainFunctionMichal1 for = [showInstructions (exportIndex11 y) ++ ", " ++ show (is_taut)
    ++ ", " ++ show (numberOfNodes (synthFlex y for)) ++ ", " ++ show (numberOfBranches (synthFlex y for)) ++ ", " ++ show (b_depthOfTree (synthFlex y for)) | y <- goodInstructions]
    where
      goodInstructions  = allRules (genBase for)
      is_taut           = Canonical.can_isTaut for




  --base = [V 0, V 1, V 2, V 3, V 4]

genBase :: For -> [For]
genBase for = [V i | i <- [0..(maxVariable for)]]

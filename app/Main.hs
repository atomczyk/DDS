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
import Parsing
import System.IO

generator1 = mkStdGen 356
fors = take 1000 $ randomRs (V 20, V 5) generator1
indexed_fors = zip [1..] fors

generator2 = mkStdGen 356
fors1 = take 1000 $ randomRs (V 15, V 4) generator2

main :: IO()
main = do
    let forss = map auxiliaryFunction fors1
    let columns = "formula" ++ ", " ++ "is_taut\n"
    writeFile "data/formulas_imp_neg.txt" columns
    appendFile "data/formulas_imp_neg.txt" $ unlines forss
--    writeFile "data/1000formulas_no_equiv.txt" $ unlines $ forss

auxiliaryFunction :: For -> String
auxiliaryFunction x = Printing.printFor x ++ ", " ++ a
  where
    a = show $ isTaut1 x


{-
main :: IO()
main = do
  let columns = "formula" ++ ", " ++ "alpha" ++ ", " ++ "beta" ++ ", " ++ "fmi\n"
  let info = map auxiliaryFunction fors
  writeFile "data/filtered_instructions/fmi.txt" columns
  appendFile "data/filtered_instructions/fmi.txt" $ unlines info

auxiliaryFunction :: For -> String
auxiliaryFunction x = Printing.printFor x ++ ", " ++ a ++ ", " ++ b ++ ", " ++ c
  where
    a = show $ alphaComplexity x
    b = show $ betaComplexity x
    c = show $ fmi x
-}

{- 1000 formuł
main :: IO()
main = do
    let formulas = [Printing.printFor (snd x) | x <- indexed_fors]
    let clean_formulas = [show (snd x) | x <- indexed_fors]
--    let g = mkStdGen 356
--    let fors = take 1000 $ randomRs (V 20, V 5) g
    writeFile "data/dla_marcina/for.txt" $ "formula\n"
    appendFile "data/dla_marcina/for.txt" $ unlines formulas
    writeFile "data/dla_marcina/for_szymon.txt" $ unlines clean_formulas
    print (head fors)
-}

{-
-- Generowanie dla Michała
main :: IO()
main = do
  dataM <- readFile "instrukcje_991.txt" --ReadMode
  let content = tail (lines dataM)
  let info = concat (map mainFunctionMichal content)
  let columns = "formula" ++ ", " ++ "good_instruction" ++ ", " ++ "is_taut" ++ ", " ++ "num_of_nodes" ++ ", " ++ "num_of_branches" ++ ", " ++ "b_depth\n"
  writeFile "data/filtered_instructions/michal.txt" columns
  appendFile "data/filtered_instructions/michal.txt" $ unlines $ info

mainFunctionMichal :: String -> [String]
mainFunctionMichal x = [(Printing.printFor formula) ++ ", " ++ showInstructions (exportIndex11 y) ++ ", " ++ show (is_taut)
  ++ ", " ++ show (numberOfNodes (synthFlex y formula)) ++ ", " ++ show (numberOfBranches (synthFlex y formula)) ++ ", " ++ show (b_depthOfTree (synthFlex y formula)) | y <- goodInstructions]
  where
    formula           = parseMM $ takeWhile (\y -> y /= ',') x
    instruction       = parseI (tail (dropWhile (\x -> x /= ',') x))
--    goodInstructions  = map showInstructions (map exportIndex11 (filterRules base instruction))
    goodInstructions  = filterRules base instruction
    is_taut           = isTaut1 formula

--preliminaryAnalysis :: String -> (For, [[[For]]])
--preliminaryAnalysis

--    showInstructions (exportIndex11 xs

--start = "\"((p->q)&r)&s\",\"p,q,s,t\",\"p,q,s,t\",\"p,q,s,t\",\"p,q,s,t\",\"r\""

base = [V 1, V 2, V 3, V 4, V 5]


mainFunction :: String -> [[[For]]]
mainFunction x = filterRules base (parseI instruction)
  where
    instruction = tail (dropWhile (\x -> x /= ',') x)

mainFunctionLength :: String -> [String]
mainFunctionLength x = [formula, instruction, show $ length (mainFunction x)]
  where
      formula = takeWhile (\y -> y /= ',') x
      instruction = init (tail (dropWhile (\x -> x /= ',') x))

basic_info :: String -> String
basic_info x = formula ++ ", " ++ instruction ++ ", " ++ number
  where
    helper = mainFunctionLength x
    formula = helper !! 0
    instruction = helper !! 1
    number = show (helper !! 2)


formula = "(((((p->q)&r)&s)&p)v(tvp))"
parsedFormula = parseMM formula
instr = "\"p,q,s,t\",\"p,q,s,t\",\"p,q,s,t\",\"p,q,s,t\",\"r\"\r"
parsedInstr = parseI Main.instr
atoms = noRepAtoms parsedFormula
goodRules = filterRules Main.atoms parsedInstr

firstList = [[V 1]]
secondList = [[V 1, V 1]]
result = filterHelper firstList secondList
-}
{-
main :: IO()
main = do
    let g = mkStdGen 356
    let fors = take 1000 $ randomRs (V 20, V 5) g
    let forss = map Printing.printFor fors
--  print $ head forss
    writeFile "data/formulas_Dawid.txt" columnsDawid
    appendFile "data/formulas_Dawid.txt" $ unlines $ dataCSV fors
--    writeFile "data/1000formulas_no_equiv.txt" $ unlines $ forss
-}

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
--print "Done"2
--          let function = \y -> (takeWhile (\x -> x /= ',') y,  tail (dropWhile (\x -> x /= ',') y))
--          let global = map (\x -> function x) content
--          firstLine <- hGetLine helloFile
--          second <- hGetLine helloFile
--          let s = content !! 1
--          let fst_s = takeWhile (\x -> x /= ',') s
--          let scn_s = tail (dropWhile (\x -> x /= ',') s)
--          let ex = head scn_s
--          parse <- read second :: [String]
--          print scn_s

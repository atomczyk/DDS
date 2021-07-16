module Main where

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

-- Aktualnie analizowane formuły
g = read "357 1" :: StdGen
gen = mkDetGenerator [I Verum Verum, N Verum]
sgen = genSamples gen

{-
-- Generowanie podzbioru zbioru wszystkich tabel dla Michała w oparciu o plik
main :: IO()
main = do
  dataM <- readFile "new_form.txt" --ReadMode
  let (fors, _) = sgen [25] [7..13] [1..5] 100 g
  let content = zip fors (tail (lines dataM))
  let info = concat (map mainFunctionMichal content)
  let info1 = take 1 info
  let columns = "formula" ++ ", " ++ "good_instruction" ++ ", " ++ "is_taut" ++ ", " ++ "num_of_nodes" ++ ", " ++ "num_of_branches" ++ ", " ++ "b_depth\n"
  writeFile "data/25_new.txt" columns
  appendFile "data/25_new.txt" $ unlines $ info1
--  print (take 1 fors)
--  print (adjustFormulaIns (((V 0) `I` (V 0)) `I` (V 1)) [[V 0],[V 1, V 1], [V 2, V 2, V 2, V 2]])
--  print result
--  let ins = [[V 1],[V 3, V 3], [V 4, V 4, V 4, V 4], [V 5, V 5, V 5, V 5,V 5, V 5, V 5, V 5], [V 2, V 2, V 2, V 2,V 2, V 2, V 2, V 2,V 2, V 2, V 2, V 2,V 2, V 2, V 2, V 2] ]
--  print (generator1)



mainFunctionMichal :: (For, String) -> [String]
mainFunctionMichal (for, x) = [(Printing.printFor for) ++ ", " ++ showInstructions (exportIndex11 y) ++ ", " ++ show (is_taut)
  ++ ", " ++ show (numberOfNodes (synthFlex (adjustFormulaIns for y) for)) ++ ", " ++ show (numberOfBranches (synthFlex (adjustFormulaIns for y) for)) ++ ", " ++ show (b_depthOfTree (synthFlex (adjustFormulaIns for y) for)) | y <- goodInstructions]
  where
--    formula           = parseMM $ takeWhile (\y -> y /= ',') x
    instruction       = parseI (tail (dropWhile (\x -> x /= ',') x))
--    goodInstructions  = map showInstructions (map exportIndex11 (filterRules base instruction))
    goodInstructions  = filterRules base instruction
--    goodInstructions  = [ins]
    is_taut           = Canonical.can_isTaut for
--    adjustedIns       = adjustFormulaIns for instruction

base = [V 0, V 1, V 2, V 3, V 4]
-}

--ins = {{1}{33}{4444}{55555555}{2222222222222222}}
--ins = [[V 1],[V 3, V 3], [V 4, V 4, V 4, V 4], [V 5, V 5, V 5, V 5,V 5, V 5, V 5, V 5], [V 2, V 2, V 2, V 2,V 2, V 2, V 2, V 2,V 2, V 2, V 2, V 2,V 2, V 2, V 2, V 2] ]

--Sprawdzić instrukcję z r na początku.

{-
generator1 = mkStdGen 356
fors = take 1000 $ randomRs (V 20, V 5) generator1
indexed_fors = zip [1..] fors
-}


--Ulitimate Desicion Tree === wersja bez rezolucji oraz KE

columns :: String
columns = "formula"
  ++ ", " ++ "tautology" ++ ", " ++ "system" ++ ", " ++ "FMI" ++ ", " ++ "alpha" ++ ", " ++ "beta"
  ++ ", " ++ "can_branches" ++ ", " ++ "can_nodes" ++ ", " ++ "can_nodes^2"
--  ++ ", " ++ "KEcan_branches"
--  ++ ", " ++ "KEcan_nodes" ++ ", "
--  ++ "KEcan_nodes^2"
  ++ ", " ++ "res_branches" ++ ", " ++ "res_nodes" ++ ", " ++ "res_nodes+num_of_res\n"
--  ++ ", " ++ "res_op_branches"
--  ++ ", " ++ "res_op_nodes"
--  ++ ", " ++ "res_op_nodes+num_of_res\n"

mainFunction :: For -> String
mainFunction x = Printing.printFor x ++ ", "
 ++ decision x ++ ","
 ++ can x ++ ", "
-- ++ ke_can x
 ++ ", " ++ res x
-- ++ ", " ++ opt_res x
  where
    can :: For -> String
    can x = (show $ Canonical.numOfSeq tree) ++ ", " ++ (show $ Canonical.lenOfCanTree tree) ++ ", " ++ (show $ (Canonical.lenOfCanTree tree)^2)
      where
        tree = Canonical.can_derFor x
    ke_can :: For -> String
    ke_can x = (show $ Canonical_KE.numOfSeq tree) ++ ", " ++ (show $ Canonical_KE.lenOfCanTree tree) ++ ", " ++ (show $ (Canonical_KE.lenOfCanTree tree)^2)
      where
        tree = Canonical_KE.ke_derFor x
    res :: For -> String
    res x = (show $ Dual.branches tree) ++ ", " ++ (show $ Dual.lenOfTree tree) ++ ", " ++ (show $ (Dual.lenOfTree tree) + (Dual.resNum tree1))
      where
        tree = Dual.derivationF x
        tree1 = Dual.proofDualtestx x
    opt_res :: For -> String
    opt_res x = (show $ Dual.branches tree) ++ ", " ++ (show $ Dual.lenOfTree tree) ++ ", " ++ (show $ (Dual.lenOfTree tree) + (Dual.resNum tree1))
      where
        tree = Dual3.derivationFrmdups x
        tree1 = Dual3.dualProof x
    decision :: For -> String
    decision x = if (for_fmi) <= 0.5 then (show taut) ++ ", Syntetyki" ++ ", " ++ (show for_fmi) ++ ", " ++ (show alp) ++ ", " ++ (show bet) else
    		if alp <= bet then (show taut) ++ ", Kanoniczny" ++ ", " ++ (show for_fmi) ++ ", " ++ (show alp) ++ ", " ++ (show bet) else (show taut) ++ ", Dualny" ++ ", " ++ (show for_fmi) ++ ", " ++ (show alp) ++ ", " ++ (show bet)
    	         where
                alp = alphaComplexity x
                bet = betaComplexity x
                taut = can_isTaut x
                for_fmi = fmi x

main :: IO()
main = do
--  let g = read "357 1"::StdGen
--  let gen = mkDetGenerator [I Verum Verum, N Verum]
--  let sgen = genSamples gen
  let (fors, _) = sgen [25] [7..13] [1..5] 100 g
  let info = map mainFunction fors
  let info1 = take 1 info
  writeFile "data/decision_tree_25_new.txt" Main.columns
  appendFile "data/decision_tree_25_new.txt" $ unlines $ info1
--  print "Done"
--  print (take 1 fors)



--Ulitimate Desicion Tree
{-
columns :: String
columns = "formula" ++ ", " ++ "tautology" ++ ", " ++ "system" ++ ", " ++ "FMI" ++ ", " ++ "alpha" ++ ", " ++ "beta"
  ++ ", " ++ "can_branches" ++ ", " ++ "can_nodes" ++ ", " ++ "can_nodes^2"
  ++ ", " ++ "KEcan_branches" ++ ", " ++ "KEcan_nodes" ++ ", " ++ "KEcan_nodes^2"
  ++ ", " ++ "res_branches" ++ ", " ++ "res_nodes" ++ ", " ++ "res_nodes+num_of_res"
  ++ ", " ++ "res_op_branches" ++ ", " ++ "res_op_nodes" ++ ", " ++ "res_op_nodes+num_of_res\n"

mainFunction :: For -> String
mainFunction x = Printing.printFor x ++ ", " ++ decision x ++ "," ++ can x ++ ", " ++ ke_can x ++ ", " ++ res x ++ ", " ++ opt_res x
  where
    can :: For -> String
    can x = (show $ Canonical.numOfSeq tree) ++ ", " ++ (show $ Canonical.lenOfCanTree tree) ++ ", " ++ (show $ (Canonical.lenOfCanTree tree)^2)
      where
        tree = Canonical.can_derFor x
    ke_can :: For -> String
    ke_can x = (show $ Canonical_KE.numOfSeq tree) ++ ", " ++ (show $ Canonical_KE.lenOfCanTree tree) ++ ", " ++ (show $ (Canonical_KE.lenOfCanTree tree)^2)
      where
        tree = Canonical_KE.ke_derFor x
    res :: For -> String
    res x = (show $ Dual.branches tree) ++ ", " ++ (show $ Dual.lenOfTree tree) ++ ", " ++ (show $ (Dual.lenOfTree tree) + (Dual.resNum tree1))
      where
        tree = Dual.derivationF x
        tree1 = Dual.proofDualtestx x
    opt_res :: For -> String
    opt_res x = (show $ Dual.branches tree) ++ ", " ++ (show $ Dual.lenOfTree tree) ++ ", " ++ (show $ (Dual.lenOfTree tree) + (Dual.resNum tree1))
      where
        tree = Dual3.derivationFrmdups x
        tree1 = Dual3.dualProof x
    decision :: For -> String
    decision x = if (for_fmi) <= 0.5 then (show taut) ++ ", Syntetyki" ++ ", " ++ (show for_fmi) ++ ", " ++ (show alp) ++ ", " ++ (show bet) else
    		if alp <= bet then (show taut) ++ ", Kanoniczny" ++ ", " ++ (show for_fmi) ++ ", " ++ (show alp) ++ ", " ++ (show bet) else (show taut) ++ ", Dualny" ++ ", " ++ (show for_fmi) ++ ", " ++ (show alp) ++ ", " ++ (show bet)
    	         where
                alp = alphaComplexity x
                bet = betaComplexity x
                taut = can_isTaut x
                for_fmi = fmi x

main :: IO()
main = do
  let g = read "357 1"::StdGen
  let gen = mkDetGenerator [I Verum Verum, N Verum]
  let sgen = genSamples gen
  let (fors, _) = sgen [25] [7..13] [1..5] 100 g
  let info = map mainFunction fors
  writeFile "decision_tree_25.txt" Main.columns
  appendFile "decision_tree_25.txt" $ unlines $ info
  print "Done"
-}


{-
-- Generowanie aktualnie analizowanych formuł dla Michała
main :: IO()
main = do
    let g = read "357 1"::StdGen
  --  g <- getStdGen
    let gen = mkDetGenerator [LanguageMbC.I LanguageMbC.Verum LanguageMbC.Verum, LanguageMbC.N LanguageMbC.Verum]
    let sgen = genSamples gen
    let (fors, _) = sgen [25] [7..13] [1..5] 100 g
    writeFile "data/synthetic/FMI_new1.txt" (unlines (map PrintingMbC.printFor fors))
--    putStrLn $ unlines $ map (show) fors

--fors1 = take 100000 $ randomRs (V 20, V 5) generator2
--fors2 = take 1000 (filter (\x -> Canonical2.isTaut1 x) fors1)
-}


{-
main :: IO()
main = do
--    g <- readFile "data/synthetic/generator.txt"
    let gen = read "357 1"::StdGen
    let fors = take 1000 $ filter (\x -> Canonical.can_isTaut x) $ randomRs (V 20, V 5) gen
    let forss = map Printing.printFor fors
--  print $ head forss
    writeFile "data/synthetic/tautologies_20_5.txt" (unlines forss)
    print (show gen)
--    writeFile "data/synthetic/generator.txt" (show g)
--    print (show gen)
--    appendFile "data/formulas_20_5.txt" $ unlines $ dataCSV fors
--    writeFile "data/1000formulas_no_equiv.txt" $ unlines $ forss
-}

--main :: IO()
--main = do
--  putStrLn
--    writeFile "data/old_formulas.txt" unlines $ show test2
--  appendFile "data/old_formulas_test.txt" $ unlines old_data

--  appendFile "data/old_formulas.txt" $ unlines $ dataCSV (take 2 (map (\x -> read x :: For) final3))



{-
--Test dla Michała - nowe sortComplexity
--(((((p&~q)&r)&(s->(r->p)))vt)&~~r)vr

a = ((((( (V 1) `A` N (V 2) ) `A` (V 3) ) `A` ( (V 4) `I` ( (V 3) `I` (V 1)))) `D` (V 5) ) `A` N (N ( (V 3) )) ) `D` (V 3)
-}
{-
-- Generowanie dla Michałas
main :: IO()
main = do
  let gen = read "357 1"::StdGen
  let fors = take 1000 $ randomRs (V 20, V 5) gen
  let forss = map Printing.printFor fors
--  dataM <- readFile "rankingi.txt" --ReadMode
--  let content = zip fors (tail (lines dataM))
--  let info = take 3 (concat (map mainFunctionMichal content))
--  let columns = "formula" ++ ", " ++ "good_instruction" ++ ", " ++ "is_taut" ++ ", " ++ "num_of_nodes" ++ ", " ++ "num_of_branches" ++ ", " ++ "b_depth\n"
--  writeFile "data/filtered_instructions/filtered_instructions_20_5.txt" columns
--  appendFile "data/filtered_instructions/filtered_instructions_20_5.txt" $ unlines $ info
  print (show (head fors))

mainFunctionMichal :: (For, String) -> [String]
mainFunctionMichal (for, x) = [(Printing.printFor for) ++ ", " ++ showInstructions (exportIndex11 y) ++ ", " ++ show (is_taut)
  ++ ", " ++ show (numberOfNodes (synthFlex y for)) ++ ", " ++ show (numberOfBranches (synthFlex y for)) ++ ", " ++ show (b_depthOfTree (synthFlex y for)) | y <- goodInstructions]
  where
--    formula           = parseMM $ takeWhile (\y -> y /= ',') x
    instruction       = parseI (tail (dropWhile (\x -> x /= ',') x))
--    goodInstructions  = map showInstructions (map exportIndex11 (filterRules base instruction))
    goodInstructions  = filterRules base instruction
    is_taut           = Canonical.isTaut1 for

--preliminaryAnalysis :: String -> (For, [[[For]]])
--preliminaryAnalysis

--    showInstructions (exportIndex11 xs

--start = "\"((p->q)&r)&s\",\"p,q,s,t\",\"p,q,s,t\",\"p,q,s,t\",\"p,q,s,t\",\"r\""

base = [V 0, V 1, V 2, V 3, V 4]


mainFunction :: String -> [[[For]]]
mainFunction x = filterRules base (parseI instruction)
  where
    instruction = tail (dropWhile (\x -> x /= ',') x)
-}


{-
mainFunctionLength :: String -> [String]
mainFunctionLength x = [formula, instruction, show $ length (mainFunction x)]
  where
      formula = takeWhile (\y -> y /= ',') x
      instruction = init (tail (dropWhile (\x -> x /= ',') x))
-}

{-
basic_info :: String -> String
basic_info x = formula ++ ", " ++ instruction ++ ", " ++ number
  where
    helper = mainFunctionLength x
    formula = helper !! 0
    instruction = helper !! 1
    number = show (helper !! 2)
-}

{-
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

--g = mkStdGen 356

{-
main :: IO()
main = do
--    g <- readFile "data/synthetic/generator.txt"
    let gen = read "357 1"::StdGen
    let fors = take 1000 $ randomRs (V 20, V 5) gen
    let forss = map Printing.printFor fors
--  print $ head forss
    writeFile "data/synthetic/formulas_20_5.txt" (unlines forss)
    print (show gen)
--    writeFile "data/synthetic/generator.txt" (show g)
--    print (show gen)
--    appendFile "data/formulas_20_5.txt" $ unlines $ dataCSV fors
--    writeFile "data/1000formulas_no_equiv.txt" $ unlines $ forss
-}

-- formulas for Marcin

{-generator2 = mkStdGen 356
fors1 = take 100000 $ randomRs (V 20, V 5) generator2
fors2 = take 1000 (filter (\x -> Canonical2.isTaut1 x) fors1)

main :: IO()
main = do
    let forss = map auxiliaryFunction fors2
    let columns = "formula" ++ ", " ++ "is_taut\n"
    writeFile "data/formulas_JD.txt" columns
    appendFile "data/formulas_JD.txt" $ unlines forss
    print (length fors2)
--    print (length $ nub fors1)
--    writeFile "data/1000formulas_no_equiv.txt" $ unlines $ forss
-}

{-
-- Dla Marcina
generator2 = mkStdGen 356
fors1 = take 1000 (randomRs (V 30, V 7) generator2)

main :: IO()
main = do
    let forss = map auxiliaryFunction fors1
    let columns = "formula" ++ ", " ++ "is_taut_can\n" -- ++ ", " ++ "is_taut_res\n" -- ++ ", " ++ "synth\n"
    writeFile "data/for_imp_neg_30_7.txt" columns
    appendFile "data/for_imp_neg_30_7.txt" $ unlines forss


auxiliaryFunction :: For -> String
auxiliaryFunction x = Printing.printFor x ++ ", " ++ a -- ++ ", " ++ b  -- ++ ", " ++ c
  where
    a = show $ Canonical.isTaut1 x
    b = show $ Dual3.isTaut x
--    c = show $ Dual.proofDualtestx x
--    c = show $ CanonicalTrees.tautology x
-}


-- Test rezolucji
{-
generator = mkStdGen 356
fors1 = take 2000 (randomRs (V 10, V 4) generator)

main :: IO()
main = do
    let forss = map auxiliaryFunction fors1
    let columns = "formula" ++ "size_dual"  ++ ", " ++ "size_dual3\n"
    writeFile "data/resolution_test.txt" columns
    appendFile "data/resolution_test.txt" $ unlines forss


auxiliaryFunction :: For -> String
auxiliaryFunction x = Printing.printFor x ++ ", " ++ a ++ ", " ++ b
  where
    a = show $ Dual3.numOfSeq $ Dual.proofDualtestx x
    b = show $ Dual3.numOfSeq $ Dual3.dualProof x
--    a1 = Dual2.proofDualtestx x
--    b = show $ Dual2.isTaut x
--    c = show $ Dual3.numOfSeq a1
--    d1 = Dual3.proofDualtestx x
--    d = show $ CanonicalTrees.tautology x
--    e = show $ Dual3.numOfSeq d1
--    c = show $ CanonicalTrees.tautology x
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

{- Generated for Marcin (formula mass index confirmation)
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
-- Test kanonicznych

generator = read "357 1"::StdGen
fors1 = take 1000 (randomRs (V 10, V 3) generator)

main :: IO()
main = do
    let forss = map auxiliaryFunction fors1
    let columns = "formula" ++ ", " ++ "is_taut_can"  ++ ", " ++ "branches_can" ++ ", " ++ "is_taut_KE_can" ++ ", " ++ "branches_KE_can" ++ ", " ++ "is_taut_res" ++ ", " ++ "branches_res\n"
    writeFile "data/KE_can_test.txt" columns
    appendFile "data/KE_can_test.txt" $ unlines forss


auxiliaryFunction :: For -> String
auxiliaryFunction x = Printing.printFor x ++ ", " ++ a ++ ", " ++ a1 ++ ", " ++ b ++ ", " ++ b1 ++ ", " ++ c ++ ", " ++ c1
  where
    a = show $ Canonical.isTaut1 x
    a1 = show $ Canonical.numOfSeq $ Canonical.derivationF x
    b = show $ Canonical2.isTaut1 x
    b1 = show $ Canonical2.numOfSeq $ Canonical2.derivationF x
    c = show $ Dual.isTaut x
    c1 = show $ Dual.branches $ Dual.proofDualtestx x
-}

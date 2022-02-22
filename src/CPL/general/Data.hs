module Data where

import Language
import AuxiliaryFormulas
import AuxiliaryTrees
import CutRule
import TheClassicalLogicRule
--import PrintingTrees
import Generator
import Printing
import FlexibleTrees
import CanonicalTrees
import Data.String
import Data.List
import Canonical
import Dual
import DecisionTree




{-
dataCSV :: [For] -> [String]
dataCSV xs = map infoNumDawid xs


columns :: String
columns = "formula" ++ ", " ++ "is_tautology" ++ ", " ++ "DT-method" ++ ", " ++
  "FMI" ++ ", " ++ "alpha" ++
  ", " ++ "beta" ++ ", " ++
  "synth-nodes" ++ ", " ++
  "synth-branches" ++ ", " ++ -- synthNodes i synthBranches mogą wybrać inne drzewo
  "can-nodes" ++ ", " ++
  "can-branches" ++ ", " ++
  "dual-nodes*" ++ ", " ++ "dual-resolution\n"

columnsDawid :: String
columnsDawid = "formula" ++ ", " ++ "is_tautology\n"

infoNumDawid :: For -> String
infoNumDawid for = printFor (for) ++ ", " ++ show (isTaut1 for) ++ "\n"
-}

{-
infoNum :: For -> String
infoNum for = printFor (for) ++ ", " ++
    dt ++ ", " ++ show fm ++ ", " ++
    show ac ++ ", " ++ show bc ++ ", " ++
    show bestN ++ ", " ++ show bestB ++ ", " ++
    show canN ++ ", " ++ show canB ++ ", " ++
    show dualN ++ ", " ++ show dualR ++ "\n"
     where
       fm = fmi for
       ac = alphaComplexity for
       bc = betaComplexity for
       bestN = bestTreeNodes for
       bestB = bestTreeBranches for
       canN = lenOfCanTree can
       canB = numOfSeq can
       can = Canonical.derivationF for
       dualN = lenOfDualTree dual
       dualR = resNum dual
       dual = proofDualtestx for
       dt  = decisionTree for 0.5
-}
--dataCSV1 :: [[String]] -> [String]
--dataCSV1 xs = concat dataCSV xs

--main :: IO()
--main = do
--  writeFile "dualtest.txt" columns
--  appendFile "dualtest.txt" $ unlines $ dataCSV Data.test



--test = map (\x -> read x :: For) (final8)


{-
test =  map (\x -> read x :: For) (final12)

info :: For -> [(For,[[For]], MT, MT)]
info for = [(for, x, synthFlex x for, synth for) | x <- ins]
    where
      ins = allRules (noRepAtoms for)
-}
--init added
--showInstruction :: [Int] -> String
--showInstruction xs  = "{" ++ show (joiner xs) ++ "}"


showInstruction :: [Int] -> String
showInstruction []	= " "
showInstruction (x:xs) = (show x) ++ showInstruction xs

showInstructions :: [[Int]] -> String
showInstructions xs = "{" ++ concat (map showInstruction xs) ++ "}"

joiner :: [Int] -> Int 
joiner = read . concatMap show


infoNum :: For -> [[For]] -> String
infoNum for xs = printFor (for) ++ "," ++ showInstructions (exportIndex11 xs) ++ "," ++
                  show (numberOfBranches tree1) ++ "," ++
                  show (depthOfTree tree1) ++ "," ++
                  show (numberOfNodes tree1) ++ "," ++
                  show (b_depthOfTree tree1) ++ "," ++
                  show (numberOfBranches tree2) ++ "," ++
                  show (depthOfTree tree2) ++ "," ++
                  show (numberOfNodes tree2) ++ "," ++
                  show (first) ++ "," ++
                  show (second) ++ "," ++
                  show (taut)
                    where
                      tree1   = synthFlex xs for
                      tree2   = synth for
                      taut    = tautology for
                      prop    = synthRate xs for
                      first   = fst prop
                      second  = snd prop



infoNumCSV :: For -> [String]
infoNumCSV for = [infoNum for x | x <- allRules (noRepAtoms for)]

dataCSV :: [For] -> [String]
dataCSV xs = concat (map infoNumCSV xs)

test = map (\x -> read x :: For) final12

test1 = take 3 test

test2 = map (\y -> read y :: For) (take 4 final12)

test3 = test2 !! 2

old_data = dataCSV test1


--dataCSV1 :: [[String]] -> [String]
--dataCSV1 xs = concat dataCSV xs

--writecol = writeFile "up_to_12.txt" columns
--writeInfo = appendFile "up_to_12.txt" $ unlines $ dataCSV test

{-

contains4atoms :: For -> Bool
contains4atoms f = length (nub (atoms f)) == 4

contains_n_atoms :: Int -> For -> Bool
contains_n_atoms n f = length (nub (atoms f)) == n

newtest = filter contains4atoms test

new_test = filter (contains_n_atoms 3) test
-}

columns :: String
columns = "formula" ++ "," ++
          "instruction" ++ "," ++
          "branches" ++ "," ++
          "depth" ++ "," ++
          "nodes" ++ "," ++
          "b-depth" ++ "," ++
          "c-branches" ++ "," ++
          "c-depth" ++ "," ++
          "c-nodes" ++ "," ++
          "F-synth" ++ "," ++
          "~F-synth" ++ "," ++
          "is tautology" ++ "\n"

{-
infoNum :: For -> [String]
infoNum for = show columns : [show [printFor (Just for), showInstructions (exportIndex11 x),
                info1 ++ show (numberOfBranches (synthFlex x for)),
                info2 ++ show (depthOfTree (synthFlex x for)),
                info3 ++ show (numberOfVertices (synthFlex x for)),
                info4 ++ show (numberOfBranches tree2),
                info5 ++ show (depthOfTree tree2),
                info6 ++ show (numberOfVertices tree2) ] | x <- ins]
    where
      tree2 = synth for
      ins = allRules (noRepAtoms for)
      info1 = "Branches: "
      info2 = "Depth: "
      info3 = "Nodes: "
      info4 = "C-Branches: "
      info5 = "C-Depth: "
      info6 = "C-Nodes: "


infoNumCSV :: For -> [String]
infoNumCSV for = [show [printFor (Just for), showInstructions (exportIndex11 x),
                 show (numberOfBranches (synthFlex x for)),
                 show (depthOfTree (synthFlex x for)),
                 show (numberOfVertices (synthFlex x for)),
                 show (b_depthOfTree (synthFlex x for)),
                 show (numberOfBranches tree2),
                 show (depthOfTree tree2),
                 show (numberOfVertices tree2) ] | x <- ins]
    where
      tree2 = synth for
      ins = allRules (noRepAtoms for)
-}

{-
infoNumCSVList :: [For] -> [[String]]
infoNumCSVList xs = map infoNumCSV xs

--experiment = writeFile "ex.txt" $ unlines $ show $ infoNumCSVList test

columns :: [String]
columns = ["Formula", "Instruction", "Branches", "Depth", "Nodes", "B-depth", "C-branches", "C-depth", "C-nodes"]
{-
writeColumns1 = writeFile "columns.txt" $ show columns

writeColumns = writeFile "columns.txt" $ drop 2 $ takeWhile (\c -> c/= ']') $ show columns
writeData = appendFile "columns.txt" $ unlines $ infoNum ((V 1) `I` ((((V 2) `A` (V 3)) `D` (V 4)) `I` (V 1)))
--infoCSV :: For -> [String]
-}
ur xs = drop 1 $ takeWhile (\c -> c/= ']') xs
{-
infoString x = map show (infoNum x)
-}
writeInfo =  writeFile "info.txt" $ unlines $ map ur (infoNumCSV ((V 1) `I` ((((V 2) `A` (V 3)) `D` (V 4)) `I` (V 1))))
-}

{-
f1 = V 1 `I` V 2
f2 = V 1 `I` V 3
f3 = V 1 `I` (V 2 `A` V 3)

f4 = ((V 1 `D` V 2) `I` V 3) `D` V 4
f5 = (V 4 `A` V 3) `I` (V 1 `I` V 2)

f6 = (V 1 `D` V 2) `A` ((V 1 `A` V 3) `I` V 2)
f7 = V 3 `I` V 4
f8 = (V 2 `A` V 1) `A` ((V 3 `D` V 4) `I` V 5)

amelia1 = f1 `I` (f2 `I` f3)
amelia2 = f4 `I` f5
formula1 = f6 `I` (f7 `I` f8)

writecolA1 = writeFile "a3.txt" columns
writeInfoA1 = appendFile "a3.txt" $ unlines $ dataCSV [formula2]

formula2 = (V 1 `I` V 2) `I` ((V 2 `I` V 3) `I` ((V 3 `I` V 4) `I` ((V 4 `I` V 5) `I` (V 1 `I` V 5) ) ) )

formula3 = (f1) `D` (f2) `D` (f3) `D` (f4) `D` (f5) `D` (f6) `D` (f7) `D` (f8) `D` (f9)
  where
    f1 = V 1 `A` V 2
    f2 = f1 `A` V 5
    f3 = V 1 `A` V 4 `A` N (V 2)
    f4 = V 1 `A` N (V 4) `A` V 5 `A` N (V 2)
    f5 = V 1 `A` N (V 4) `A` N (V 5) `A` N (V 2)
    f6 = V 1 `A` N (V 4) `A` N (V 5) `A` N (V 2) `A` V 4
    f7 = N (V 1) `A` N (V 3) `A` V 5
    f8 = N (V 1) `A` N (V 3) `A` N (V 5)
    f9 = N (V 1) `A` N (V 3) `A` V 5 `A` V 1

formula4 = (V 1 `I` V 1) `I` V 3 `I` V 2 `I` ((V 3 `I` V 5) `I` (N (V 1) `I` V 4 ) ) `I` V 4 `I` N (V 3)

formula5 = f1 `A` f2 `A` f3 `A` f4 `A` f5 `A` f6 `A` f7 `A` f8 `A` f9
  where
    f1 = V 1 `D` V 3 `D` V 5
    f2 = V 1 `D` V 3 `D` N (V 5) `D` V 2
    f3 = V 1 `D` V 3 `D` N (V 5) `D` N (V 2)
    f4 = N (V 1) `D` V 3 `D` V 4 `D` V 2
    f5 = N (V 1) `D` V 3 `D` V 4 `D` N (V 2)
    f6 = N (V 1) `D` V 3
    f7 = N (V 3) `D` V 4
    f8 = N (V 3) `D` N (V 4) `D` N (V 1)
    f9 = N (V 3) `D` N (V 4)

-}

{-
    infoNum :: For -> String
    infoNum for = printFor (for) ++ ", " ++ show (tautDual) ++ ", " ++ show (tautCan)
       where
         tautDual = show (isTaut for)
         tautCan = show (isTaut1 for)


    dataCSV :: [For] -> [String]
    dataCSV xs = map infoNum xs


    columns :: String
    columns = "formula" ++ ", " ++
        "is tautology Dual" ++ ", " ++
    	  "is tautology Can" ++  " \n "

    --dataCSV1 :: [[String]] -> [String]
    --dataCSV1 xs = concat dataCSV xs

    main :: IO()
    main = do
      writeFile "dualtest.txt" columns
      appendFile "dualtest.txt" $ unlines $ dataCSV Data.test



    test = map (\x -> read x :: For) (final8)

    problem = map (proofDualtestx) (take 200 Data.test)
-}

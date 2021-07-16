module FlexibleTrees
  (
    allInstructions
  , insttructionsInternal
  , allRules
  , filterRules
  , filterHelper
  , adjustFormulaIns
  , subset
  , preCanonicalTreeFlex
  , synthAccumFlex
  , applyRuleFlex
  , synthFlex
  , allTreesFlex
  , allTFlex
  , synthRate
  , tautologyFlex
  , bestTreeNodes
  , bestTreeBranches
  )
where

import Language
import AuxiliaryFormulas
import AuxiliaryTrees
import CutRule
import TheClassicalLogicRule
import PrintingTrees
import Data.Tree
import Data.List
import Control.Monad
import Generator
import OptimizedTrees

allInstructions :: [For] -> [[[For]]]
allInstructions [x]     = [[[x]]]
allInstructions (x:xs)  = [[x] : (mergeFlex (a) (b)) | a <- base, b <- base]
     where
       base = insttructionsInternal xs
       mergeFlex :: [[a]] -> [[a]] -> [[a]]
       mergeFlex [] []          = [[]]
       mergeFlex (x:xs) (y:ys)  = (x ++ y):mergeFlex xs ys

insttructionsInternal :: [For] -> [[[For]]]
insttructionsInternal xs = nub $ concat $ map allInstructions (permuteAtoms xs)

allRules :: [For] -> [[[For]]]
allRules [x] = [[[x]]]
allRules xs = map (filter (/= [])) $
              concat [allInstructions (x:y) | x <- xs, y <- [delete x xs]]

-- Filter instructions according to Michal's heuristics
filterRules :: [For] -> [[For]] -> [[[For]]]
filterRules x y = [ins | ins <- (allRules x), filterHelper y ins]

filterHelper :: [[For]] -> [[For]] -> Bool
filterHelper ys []         = True
filterHelper [] ys         = True
filterHelper (x:xs) (y:ys) = (subset y x) && filterHelper xs ys

subset a b = null [x | x<-a ,elem x b == False]

maxVariable :: For -> Int
maxVariable x = case x of
  (V i)  -> i+1
  (N p)  -> maxVariable p
  (I p q)  -> maximum (maxVariable p, maxVariable q)

adjustFormulaIns :: For -> [[For]] -> [[For]]
adjustFormulaIns for xs = take (maxVariable for) xs

{-
helper :: [For] -> [For] -> Bool
helper [] ys = True
helper (x:xs) ys = if x `elem` ys then True else helper xs ys
-}


a = allRules ([V 1, V 2, V 3, V 4, V 5])
--acount = count ([[V 2], [V 1, V 1], [V 3, V 3, V 3, V 3], [V 4, V 4, V 4, V 4, V 4, V 4, V 4, V 4], []]) a

-- | Builds a canonical tree given a sequence of instructions. Given an appropriate instruction
--   this tree is identical to the one created by buildPreCanonicalTree.
preCanonicalTreeFlex :: [[For]] -> MT -> MT
preCanonicalTreeFlex [] tree = tree
preCanonicalTreeFlex (x:xs) tree  = preCanonicalTreeFlex xs (cutRuleFlex x tree)

-- | Optimized flexible tree. The compulsory strategy is used. All formulas are synthesized
-- as soon as possible. Tail recursion is used. The first argument is the goal formula,
-- the second is a list of instructions for every level of a tree and the third
-- argument is a list of formulas, which will be synthesized.
synthAccumFlex :: For -> [[For]] -> [For] -> MT -> MT
synthAccumFlex for (x:xs) [] tree     = cutRuleFlex1 x for tree -- added instead of `tree`, seems to work but reconsider, cutRuleFlex1 x for tree
synthAccumFlex for [] (y:ys) tree     = tree
synthAccumFlex for (x:xs) (y:ys) tree = synthAccumFlex for xs (y:ys) $
                                        applyRuleFlex for (y:ys) $
                                        cutRuleFlex1 x for tree

complete :: For -> MT -> Bool    -- check if a tree sytnthesizes for or ~for on each branch.
complete for (Node x [])      = (Just for) `elem` x || (Just (N for)) `elem` x
complete for (Node x (y:ys))  = and (map (complete for) (y:ys))

-- | A version of `applyRule` needed in `synthAccumFlex`.
applyRuleFlex :: For -> [For] -> MT -> MT
applyRuleFlex for [] tree     = tree
applyRuleFlex for (x:xs) tree = applyRuleFlex for xs $                    -- dodane (x:xs)
                                actOnLeaves' for (synthesizeFor) x tree

-- | Given a sequence of instructions, synthFlex synthesize for.
synthFlex :: [[For]] -> For -> MT
synthFlex fors for = synthAccumFlex for fors subformulas (Node [Nothing] [])
  where
    subformulas = sortComplexity (subFormulas for)

-- | Generate all possible synthetic tableaux (using compulsory strategy).
-- The second argument is a list of lists of atoms which govern the order of cuts.
allTreesFlex :: For -> [[[For]]] -> [MT]
allTreesFlex for []     = []
allTreesFlex for (x:xs) = synthAccumFlex for x subformulas (Node [Nothing] []): allTreesFlex for xs
      where
        subformulas = sortComplexity (subFormulas for)

allTFlex :: For -> [MT]
allTFlex for = allTreesFlex for (allRules (noRepAtoms for))

-- | Selects best tree with respect to the number of nodes
bestTreeNodes :: For -> Int
bestTreeNodes for = best $ allTFlex for
  where
    best :: [MT] -> Int
    best xs = minimum $ map numberOfNodes xs

-- | Selects best tree with respect to the number of branches
bestTreeBranches :: For -> Int
bestTreeBranches for = best $ allTFlex for
  where
    best :: [MT] -> Int
    best xs = minimum $ map numberOfBranches xs

-- | Information about the number of leaves with A and not-A
synthRate :: [[For]] -> For -> (Int, Int)
synthRate fors for = calculate for (synthFlex fors for)

-- | How many times A and ~A have been synthesized
calculate :: For -> MT -> (Int, Int)
calculate for tree = (succes, failure)
    where
      list    = map last (leaves tree)
      succes  = length (filter (== Just for) list)
      failure = (length list) - succes

log1 = (V 1) `I` ((N (V 1)) `I` (V 2))
log2 = [[V 1],[V 2, V 2]]
log3 = synthRate log2 log1

-- | Check whether a formula is a tautology.
tautologyFlex :: For -> Bool
tautologyFlex for = all ((Just for) `elem`) (leaves first_tree)
  where
    first_tree = head (allTFlex for)

mrate = synthRate [[V 1],[V 2, V 2]] m2
mrate1 = length $ branches $ synthFlex [[V 1],[V 2, V 2]] m2


-- Przykłady Michala -- obie taut
-- ((p2) -> (~(p1))) -> ((p1) -> (~(~(~(~(p1))))))" - 169 - old=1, new=0
m = ((V 2) `I` N (V 1)) `I` ((V 1) `I` N (N ( N ( N (V 1)))))

-- "~(~(~(~((~(~(p1))) -> ((~(p1)) -> (p2))))))" - 413 - old=0, new=1
m1 = N (N (N (N ((N (N (V 1))) `I` ((N (V 1)) `I` (V 2))))))


-- Problematyczne formulaComplexity
-- 1. "~(~((~(~(p1))) -> ((~(~(~(p1)))) -> (p2))))"  - not-A-synth = 0, ale nie jest taut (wg ostatniej wersji)

m2 =  (N (N ((N (N (V 1))) `I` ((N (N (N (V 1)))) `I` (V 2)))))



--Examples, Problematic cases
--((p5) -> (p1)) & (p4)
-- {5}{14}{4411}
--instruction = [[V 5], [V 1, V 4], [V 4, V 4, V 1, V 1]]
--formula = ((V 5) `I` (V 1)) `A` (V 4)
--der = synthFlex instruction formula

ins = [[V 3],[V 2, V 1],[V 1, V 1, V 2, V 2]]
for = (V 1) `A` ((V 2) `I` (V 3))
der = synthFlex ins for
derr = putStrLn $ drawTree (prePrint1 der)

ins1 = [[V 1], [V 2, V 2]]
for1 = N ((V 1) `I` (V 2))
der1 = synthFlex ins1 for1
derr1 = putStrLn $ drawTree (prePrint1 der1)
{-
main :: IO [()]
main = do
        let fors    = map (\x -> read x :: For) final3
        let numbers = map show [1..(length fors)]
        let trees   = map show (map allTFlex fors)
        zipWithM writeFile numbers trees

main1 :: IO [()]
main1 = do
          let fors    = map show [1,2]
          let numbers = map show
          zipWithM writeFile fors numbers1


q = map unlines (map (map show) [[1,1],[2,2]])

aFormula = (I (N (V 1)) (D (V 2) (N (V 3) ) ) )
aTrees = allTFlex aFormula
allTrees = map (drawTree.prePrint1) (aTrees)
allTreesA = writeFile "all3trees.txt" $ "All possible trees for a formula ~p1 -> (p2 v ~p3)\n"
allTreesAA = appendFile "all3trees.txt" (unlines allTrees)

aFormula1 = (I (A (N (V 1)) (V 4) ) (D (V 2) (N (V 3) ) ) )
aTrees1 = allTFlex aFormula1
allTrees1 = map (drawTree.prePrint1) (aTrees1)
allTreesA1 = writeFile "all4trees.txt" $ "All possible trees for a formula (~p1 ^ p4) -> (p2 v ~p3)\n"
allTreesAA1 = appendFile "all4trees.txt" (unlines allTrees1)
-}

module OptimizedTrees
  (
    synthAccum1
  , applyRule11
  , synth11
  , tautology1
  , allTrees
  , allT
  , allTT
  )
where

import Language
import AuxiliaryFormulas
import AuxiliaryTrees
import CutRule
import TheClassicalLogicRule
import CanonicalTrees
import Data.Tree
import Data.List

-- | Optimized tree. The compulsory strategy is used. All formulas are synthesized
-- as soon as possible. Tail recursion is used.
synthAccum1 :: For -> [For] -> [For] -> MT -> MT
synthAccum1 for (x:xs) [] tree     = tree
synthAccum1 for [] (y:ys) tree     = tree
synthAccum1 for (x:xs) (y:ys) tree = synthAccum1 for xs (y:ys) (applyRule11 for (y:ys) (cutRule1 for x tree))

-- | A version of `applyRule1` needed in `synthAccum1`.
applyRule11 :: For -> [For] -> MT -> MT
applyRule11 for [] tree     = tree
applyRule11 for (x:xs) tree = applyRule11 for xs (actOnLeaves' for (synthesizeFor) x tree)

-- | Synthesize a formula. Compulsory strategy is used. If the list of atoms is
-- apropriately chosen (using functions designed by DLJ), the generated tree is a minimal
-- proof, such that the order of cuts on each branch is the same.
synth11 :: For -> MT
synth11 for = synthAccum1 for cutformulas subformulas (Node [Nothing] [])
  where
    cutformulas = noRepAtoms for
    subformulas = sortComplexity (subFormulas for)

-- | Check whether a formula is a tautology.
tautology1 :: For -> Bool
tautology1 for = all ((Just for) `elem`) (leaves $ synth11 for)

-- | Generate all possible synthetic tableaux (using compulsory strategy).
-- The second argument is a list of lists of atoms which govern the order of cuts.
allTrees :: For -> [[For]] -> [MT]
allTrees for []     = []
allTrees for (x:xs) = synthAccum1 for x subformulas (Node [Nothing] []): allTrees for xs
      where
        subformulas = sortComplexity (subFormulas for)

-- | Specification of `allTrees`.
allT :: For -> [MT]
allT for = allTrees for (permuteAtoms (noRepAtoms for))

-- | All possible tableaux + the information about their complexity.
allTT for = zip (numBranch (allT for)) (permuteAtoms (noRepAtoms for))

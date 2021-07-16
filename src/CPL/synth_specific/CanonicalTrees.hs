module CanonicalTrees
  (
    preCanonicalTree
  , buildPreCanonicalTree
  , applyRule1
  , synth
  , tautology
  )
where

import Language
import AuxiliaryFormulas
import AuxiliaryTrees
import CutRule
import TheClassicalLogicRule
import Data.Tree
import Data.List

-- | Generate a tree (`precanonical`) with all possible cuts.
preCanonicalTree :: [For] -> MT -> MT
preCanonicalTree fors y = case fors of
  []      -> y
  (z:zs)  -> preCanonicalTree zs $ cutRule z y

-- | Generate precanonical tree for atoms in a given formula.
buildPreCanonicalTree :: For -> MT
buildPreCanonicalTree for = preCanonicalTree (noRepAtoms for) $
                          Node [Nothing] []

-- | Specification of `actOnLeaves` - the classical rule is used.
applyRule1 :: For -> For -> MT -> MT
applyRule1 goal for tree = actOnLeaves' goal (synthesizeFor) for tree

-- | Synthesize a formula. The result is a canonical synthetic tableaux, which
-- is usually not the shortest one.
synth :: For -> MT
synth for = synthAccum (sub) (tree)
    where
      sub  = sortComplexity (subFormulas for)
      tree = buildPreCanonicalTree for

      synthAccum :: [For] -> MT -> MT
      synthAccum [] accum     = accum
      synthAccum (x:xs) accum = synthAccum xs (applyRule1 for x accum)

-- | Check whether a formula is a tautology.
tautology :: For -> Bool
tautology for = all ((Just for) `elem`) (leaves $ synth for)

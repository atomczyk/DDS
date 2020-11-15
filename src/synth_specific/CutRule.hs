module CutRule
  (
    cutFormula
  , cutRule
  , cutRule1
  , cutRuleFlex
  , cutRuleFlex1
  , halve
  )
where

import Language
import Data.Tree

-- | Cut formula.
cutFormula :: For -> (For, For)
cutFormula = \x -> (x, N x)

-- | Classical cut rule.
cutRule :: For -> MT -> MT
cutRule for y = case y of
    Node z []  -> Node z [Node (formula:z) [], Node (formula':z) []]
                      where
                        formula   = Just (fst $ cutFormula for)
                        formula'  = Just (snd $ cutFormula for)
    Node z ys  -> Node z (map (cutRule for) ys)

-- | Classical cut rule used in optimized version. When applying cut we do not
-- forget about the goal formula (the first argument of the function). This prevents
-- unnecessary cuts.
cutRule1 :: For -> For -> MT -> MT
cutRule1 goal for y = case y of
    Node z []  -> case (Just goal) `elem` z of
                    True  -> Node z []
                    False -> Node z [Node (z ++ [formula]) [], Node (z ++ [formula']) []]
                                      where
                                        formula   = Just (fst $ cutFormula for)
                                        formula'  = Just (snd $ cutFormula for)
    Node z ys  -> Node z (map (cutRule1 goal for) ys)

-- |  Flexible cut-rule. The first argument is an elament of instruction, which tells us
--    which formulas has to be introduced.
cutRuleFlex :: [For] -> MT -> MT
cutRuleFlex []  tree              = tree
cutRuleFlex [x] (Node z [])       = Node z [Node (formula:z) [], Node (formula':z) []]
    where
      formula   = Just (fst $ cutFormula x)
      formula'  = Just (snd $ cutFormula x)
-- cutRuleFlex xs (Node z [s1, s2])  = Node z [cutRuleFlex (fst.halve $ xs) s1, cutRuleFlex (snd.halve $ xs) s2]

-- | Classical cut rule used in optimized and flexible version. When applying cut we do not
-- forget about the goal formula (the first argument of the function). This prevents
-- unnecessary cuts.
cutRuleFlex1 :: [For] -> For -> MT -> MT
cutRuleFlex1 fors goal (Node z [])        = case ((Just goal) `elem` z || (Just (N goal)) `elem` z)  of
                                              True  -> Node z []
                                              False -> cutRuleFlex fors (Node z [])
cutRuleFlex1 fors goal (Node z [s1])      = Node z [cutRuleFlex1 fors goal s1]
cutRuleFlex1 fors goal (Node z [s1, s2])  = Node z [cutRuleFlex1 (fst (halve fors)) goal s1,
                                   cutRuleFlex1 (snd (halve fors)) goal s2]

-- | Breaks a list at halve.
halve :: [a] -> ([a],[a])
halve xs  = (take ((length xs) `div` 2 ) xs, drop ((length xs) `div` 2 ) xs)

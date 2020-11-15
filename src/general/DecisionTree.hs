module DecisionTree where

import Language
import AuxiliaryFormulas
import AuxiliaryTrees
import AuxiliaryFunctions
import CutRule
import TheClassicalLogicRule
--import PrintingTrees
import Generator
import Printing
import FlexibleTrees
import OptimizedTrees
import CanonicalTrees
import Data.String
import Data.List
import Canonical
import Dual

alphaComplexity :: For -> Int
alphaComplexity x = case x of
	V _     -> 1
	N (V _) -> 1
	N (N p) -> alphaComplexity p
	A p q   -> alphaComplexity p + alphaComplexity q
	N (D p q) -> alphaComplexity (N p) + alphaComplexity (N q)
	N (I p q) -> alphaComplexity p + alphaComplexity (N q)
	E p q -> alphaComplexity (A (I p q) (I q p))
	D p q -> alphaComplexity p * alphaComplexity q
	I p q -> alphaComplexity (N p) * alphaComplexity q
	N (A p q) -> alphaComplexity (N p) * alphaComplexity (N q)
	N (E p q) -> alphaComplexity (N (A (I p q) (I q p)))

betaComplexity :: For -> Int
betaComplexity x = case x of
		V _     -> 1
		N (V _) -> 1
		N (N p) -> betaComplexity p
		A p q   -> betaComplexity p * betaComplexity q
		N (D p q) -> betaComplexity (N p) * betaComplexity (N q)
		N (I p q) -> betaComplexity p * betaComplexity (N q)
		E p q -> betaComplexity (A (I p q) (I q p))
		D p q -> betaComplexity p + betaComplexity q
		I p q -> betaComplexity (N p) + betaComplexity q
		N (A p q) -> betaComplexity (N p) + betaComplexity (N q)
		N (E p q) -> betaComplexity (N (A (I p q) (I q p)))

-- | Number  of all of the atoms in the formula.
diffvar :: For -> Int
diffvar x = length (rmdups (atoms x))

-- | Number of different atoms in the formula.
occurrvar :: For -> Int
occurrvar x = length (atoms x)

-- | FMI function
fmi :: For -> Float
fmi x = fromIntegral (diffvar x) / fromIntegral (occurrvar x)


decisionTree :: For -> Float -> String
decisionTree x n = if fmi1 <= n then show (tautologyFlex x) ++ ", Syntetyki" else
		if alp <= bet then show (isTaut1 x) ++ ", Kanoniczny"  else show (isTaut x) ++ ", Dualny"
	where
		fmi1 = fmi x
		alp = alphaComplexity x
		bet = betaComplexity x

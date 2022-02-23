module Canonical where

import Language
--import LanguageMbC
import Data.Tree
import Sort
import Printing
import AuxiliaryFunctions
import AuxiliaryFormulas



-- | Insert a singular formula in a proper list in a sequent, depending on its form.
can_insert :: For -> CanSeq -> CanSeq
can_insert f (Can (xs, ys, zs))
	| var f     = Can (f:xs, ys, zs)
	| beta f    = Can (xs, ys, f:zs)
	| otherwise = Can (xs, f:ys, zs)


-- | Branching rules for logical connectives in canonical calculus.
can_ruleAlpha :: CanSeq -> [CanSeq]
can_ruleAlpha (Can (xs, y:ys, zs)) = case y of
      A n m -> [(can_insert n (Can (xs,ys, zs))), (can_insert m (Can (xs, ys, zs)))]
      E n m -> [(can_insert (N m) (can_insert n (Can (xs,ys, zs)))), (can_insert (N n) (can_insert m (Can (xs, ys, zs))))]
      N (E n m) -> [(can_insert m (can_insert n (Can (xs,ys, zs)))), (can_insert n (can_insert m (Can (xs, ys, zs))))]
      N (I n m) -> [(can_insert n (Can (xs,ys, zs))), (can_insert (N m) (Can (xs, ys, zs)))]
      N (D n m) -> [(can_insert (N n) (Can (xs,ys, zs))), (can_insert (N m) (Can (xs, ys, zs)))]
      N (N n)   -> [can_insert n (Can (xs,ys, zs))]


-- | Non-branching rules for logical connectives in canonical calculus.
can_ruleBeta :: CanSeq -> [CanSeq]
can_ruleBeta (Can (xs, ys, z:zs)) = case z of
      I n m -> [can_insert m (can_insert (N n) (Can (xs,ys, zs)))]
      D n m -> [can_insert m (can_insert n (Can (xs,ys, zs)))]
      N (A n m) -> [can_insert (N m) (can_insert (N n) (Can (xs,ys, zs)))]
      E n m -> [(can_insert (I n m) (Can (xs,ys, zs))), (can_insert (I m n) (Can (xs, ys, zs)))]
      N (E n m) -> [(can_insert m (can_insert n (Can (xs,ys, zs)))), (can_insert n (can_insert m (Can (xs, ys, zs))))]
      N (N n)   -> [can_insert n (Can (xs,ys, zs))]


-- | Checks if a given sequent is an atomic one, that is, there are only variables left.
can_atomicseq :: CanSeq -> Bool
can_atomicseq (Can (x, y, z)) = if ((null y && null z) || checkform y z) then True else False

-- | Check if a given sequent is a closed one, that is, there are corresponding formulas within in.
can_closedseq :: CanSeq -> Bool
can_closedseq (Can (x, y, z)) = checkform x x || checkform y z



-- | Apply a given rule to a singular sequent depending on the formulas it is comprised of.
can_applyruleSeq :: CanSeq -> [CanSeq]
can_applyruleSeq (Can (x, y, z))
		| can_atomicseq (Can (x, y, z))  = [Can (x, y, z)]
		| not (null z) 		   = can_ruleBeta (Can (x, y, z))
		| otherwise 		   = can_ruleAlpha (Can (x, y, z))

-- | Applying function cann_applyruleSeq on a given hypersequent.
can_applyruleHyp :: [CanSeq] -> [CanSeq]
can_applyruleHyp (x:xs) = if can_atomicseq x then (x:can_applyruleHyp xs) else (can_applyruleSeq x) ++ xs
can_applyruleHyp [] = []

-- | Build a tree using rule applying rules to a hypersequent.
can_buildTreeSeq :: CanSeq -> Tree [CanSeq]
can_buildTreeSeq x = Node [x] [Node (can_applyruleSeq x) []]

-- | Builds a tree for an argument of a tree type.
can_buildTree :: Tree [CanSeq] -> Tree [CanSeq]
can_buildTree (Node x [])
    	| all can_atomicseq x = (Node x [])
        | not (all can_atomicseq x) = (Node x [(Node (can_applyruleHyp x) [])])
can_buildTree (Node x xs) = Node x (map can_buildTree xs)

-- | Check if a tree is an atomic one -- all of the leaves are atomic.
can_atomTree :: Foldable t => Tree (t CanSeq) -> Bool
can_atomTree (Node x []) = if all can_atomicseq x then True else False
can_atomTree (Node x [z]) = can_atomTree z

-- | Build a derivation for a hypersequent.
can_derHyp :: [CanSeq] -> Tree [CanSeq]
can_derHyp x = until (can_atomTree) can_buildTree (Node x [])

-- | Using an above function derivation, derivation tree is returned for a singular formula.
can_derFor :: For -> Tree [CanSeq]
can_derFor x = can_derHyp [sortCan x]

-- | Check if a tree is a proof.
can_isProof :: Tree [CanSeq] -> Bool
can_isProof (Node x []) = and [can_closedseq y | y <- x]
can_isProof (Node x rest) = and $ map can_isProof rest

-- | Check if a formula is a tautology.
can_isTaut :: For -> Bool
can_isTaut for = can_isProof $ can_derFor for


-- | Number of sequents in final hypersequent.
numOfSeq :: Tree [CanSeq] -> Int
numOfSeq (Node x []) = length x
numOfSeq (Node x rest) = numOfSeq (head rest)

-- | Length of the derivation.
lenOfCanTree :: Tree [CanSeq] -> Int
lenOfCanTree (Node x []) = 1
lenOfCanTree (Node x rest) = 1 + lenOfCanTree (head rest)

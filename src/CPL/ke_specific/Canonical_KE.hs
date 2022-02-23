module Canonical_KE where

import Language
--import LanguageMbC
import Data.Tree
import Sort
import Printing
import AuxiliaryFunctions
import AuxiliaryFormulas



-- | ke_insert a singular formula in a proper list in a sequent, depending on its form.
ke_insert :: For -> CanSeq -> CanSeq
ke_insert f (Can (xs, ys, zs))
	| var f     = Can (f:xs, ys, zs)
	| beta f    = Can (xs, ys, f:zs)
	| otherwise = Can (xs, f:ys, zs)


-- | Branching rules for logical connectives in canonical calculus.
ke_ruleAlpha :: CanSeq -> [CanSeq]
ke_ruleAlpha (Can (xs, y:ys, zs)) = case y of
      A n m -> [(ke_insert n (Can (xs,ys, zs))), ke_insert (N n) ((ke_insert m (Can (xs, ys, zs))))]
      E n m -> [(ke_insert (N m) (ke_insert n (Can (xs,ys, zs)))), (ke_insert (N n) (ke_insert m (Can (xs, ys, zs))))]
      N (E n m) -> [(ke_insert m (ke_insert n (Can (xs,ys, zs)))), ((ke_insert n (ke_insert m (Can (xs, ys, zs)))))]
      N (I n m) -> [(ke_insert n (Can (xs,ys, zs))), ke_insert (N n) ((ke_insert (N m) (Can (xs, ys, zs))))] --zmiana
      N (D n m) -> [(ke_insert (N n) (Can (xs,ys, zs))), ke_insert n ((ke_insert (N m) (Can (xs, ys, zs))))] --zmiana
      N (N n)   -> [ke_insert n (Can (xs,ys, zs))]


-- | Non-branching rules for logical connectives in canonical calculus.
ke_ruleBeta :: CanSeq -> [CanSeq]
ke_ruleBeta (Can (xs, ys, z:zs)) = case z of
      I n m -> [ke_insert m (ke_insert (N n) (Can (xs,ys, zs)))]
      D n m -> [ke_insert m (ke_insert n (Can (xs,ys, zs)))]
      N (A n m) -> [ke_insert (N m) (ke_insert (N n) (Can (xs,ys, zs)))]
      E n m -> [(ke_insert (I n m) (Can (xs,ys, zs))), (ke_insert (I m n) (Can (xs, ys, zs)))]
      N (E n m) -> [(ke_insert m (ke_insert n (Can (xs,ys, zs)))), (ke_insert n (ke_insert m (Can (xs, ys, zs))))]
      N (N n)   -> [ke_insert n (Can (xs,ys, zs))]


-- | Checks if a given sequent is an atomic one, that is, there are only variables left.
ke_atomicseq :: CanSeq -> Bool
ke_atomicseq (Can (x, y, z)) = if ((null y && null z) || checkform y z) then True else False

-- | Check if a given sequent is a closed one, that is, there are corresponding formulas within in.
ke_closedseq   :: CanSeq -> Bool
ke_closedseq   (Can (x, y, z)) = checkform x x || checkform y z

-- | Is the atomic part of the sequent closed.
ke_atom_closedseq     :: CanSeq -> Bool
ke_atom_closedseq     (Can (x, y, z)) = not (checkform x x) && (y == []) && (z == [])


-- | Apply a given rule to a singular sequent depending on the formulas it is comprised of.
ke_applyRuleSeq :: CanSeq -> [CanSeq]
ke_applyRuleSeq (Can (x, y, z))
		| ke_atomicseq (Can (x, y, z))  = [Can (x, y, z)]
		| not (null z) 		   = ke_ruleBeta (Can (x, y, z))
		| otherwise 		   = ke_ruleAlpha (Can (x, y, z))

-- | Applying function ke_applyRuleSeq on a given hypersequent.
ke_applyRuleH  :: [CanSeq] -> [CanSeq]
ke_applyRuleH  (x:xs) = if ke_atomicseq x then (x:ke_applyRuleH  xs) else (ke_applyRuleSeq x) ++ xs
ke_applyRuleH  [] = []

-- | Build a tree using rule applying rules to a hypersequent.
ke_buildTreeSeq :: CanSeq -> Tree [CanSeq]
ke_buildTreeSeq x = Node [x] [Node (ke_applyRuleSeq x) []]

-- | Builds a tree for an argument of a tree type.
ke_buildTree :: Tree [CanSeq] -> Tree [CanSeq]
ke_buildTree (Node x [])
    	| all ke_atomicseq x = (Node x [])
        | not (all ke_atomicseq x) = (Node x [(Node (ke_applyRuleH  x) [])])
ke_buildTree (Node x xs) = Node x (map ke_buildTree xs)

-- | Check if a tree is an atomic one -- all of the leaves are atomic.
ke_atom_tree :: Foldable t => Tree (t CanSeq) -> Bool
ke_atom_tree (Node x []) = if all ke_atomicseq x then True else False
ke_atom_tree (Node x [z]) = ke_atom_tree z

-- | Build a derivation for a hypersequent.
ke_derHyp :: [CanSeq] -> Tree [CanSeq]
ke_derHyp x = until (ke_atom_tree) ke_buildTree (Node x [])

-- | Using an above function derivation, derivation tree is returned for a singular formula.
ke_derFor :: For -> Tree [CanSeq]
ke_derFor x = ke_derHyp [sortCan x]

-- | Check if a tree is a proof.
ke_isProof:: Tree [CanSeq] -> Bool
ke_isProof (Node x []) = and [ke_closedseq   y | y <- x]
ke_isProof (Node x rest) = and $ map ke_isProof rest

-- | Check if a formula is a tautology.
ke_isTaut:: For -> Bool
ke_isTaut for = ke_isProof $ ke_derFor for

-- | Number of sequents in final hypersequent.
numOfSeq :: Tree [CanSeq] -> Int
numOfSeq (Node x []) = length x
numOfSeq (Node x rest) = numOfSeq (head rest)

-- | Length of the full derivation.
lenOfCanTree :: Tree [CanSeq] -> Int
lenOfCanTree (Node x []) = 1
lenOfCanTree (Node x rest) = 1 + lenOfCanTree (head rest)

-- | Checks if there is any open atomic sequent in the list.
ke_fstOpen :: [CanSeq] -> Bool
ke_fstOpen xs = any (ke_atom_closedseq) xs

-- | Number of sequents in final hypersequent consisting of the first open atomic sequent.
ke_numOfSeq :: Tree [CanSeq] -> Int
ke_numOfSeq (Node x []) = length x
ke_numOfSeq (Node x rest) = if (ke_fstOpen x) then length x else numOfSeq (head rest)

-- | Length of the derivation until the first open atomic sequent.
ke_lenOfCanTree :: Tree [CanSeq] -> Int
ke_lenOfCanTree (Node x []) = 1
ke_lenOfCanTree (Node x rest) = if (ke_fstOpen x) then 1 else 1 + ke_lenOfCanTree (head rest)

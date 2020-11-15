module Canonical where

import Language
import Data.Tree
import Sort
import Printing
import AuxiliaryFunctions
import AuxiliaryFormulas


-- | Insert a singular formula in a proper list in a sequent, depending on its form.
insert :: For -> CanSeq -> CanSeq
insert f (Can (xs, ys, zs))
	| var f     = Can (f:xs, ys, zs)
	| beta f    = Can (xs, ys, f:zs)
	| otherwise = Can (xs, f:ys, zs)


-- | Branching rules for logical connectives in canonical calculus.
ruleAlpha :: CanSeq -> [CanSeq]
ruleAlpha (Can (xs, y:ys, zs)) = case y of
      A n m -> [(insert n (Can (xs,ys, zs))), (insert m (Can (xs, ys, zs)))]
      E n m -> [(insert (N m) (insert n (Can (xs,ys, zs)))), (insert (N n) (insert m (Can (xs, ys, zs))))]
      N (E n m) -> [(insert m (insert n (Can (xs,ys, zs)))), (insert n (insert m (Can (xs, ys, zs))))]
      N (I n m) -> [(insert n (Can (xs,ys, zs))), (insert (N m) (Can (xs, ys, zs)))]
      N (D n m) -> [(insert (N n) (Can (xs,ys, zs))), (insert (N m) (Can (xs, ys, zs)))]
      N (N n)   -> [insert n (Can (xs,ys, zs))]


-- | Non-branching rules for logical connectives in canonical calculus.
ruleBeta :: CanSeq -> [CanSeq]
ruleBeta (Can (xs, ys, z:zs)) = case z of
      I n m -> [insert m (insert (N n) (Can (xs,ys, zs)))]
      D n m -> [insert m (insert n (Can (xs,ys, zs)))]
      N (A n m) -> [insert (N m) (insert (N n) (Can (xs,ys, zs)))]
      E n m -> [(insert (I n m) (Can (xs,ys, zs))), (insert (I m n) (Can (xs, ys, zs)))]
      N (E n m) -> [(insert m (insert n (Can (xs,ys, zs)))), (insert n (insert m (Can (xs, ys, zs))))]
      N (N n)   -> [insert n (Can (xs,ys, zs))]


-- | Checks if a given sequent is an atomic one, that is, there are only variables left.
atomicseq :: CanSeq -> Bool
atomicseq (Can (x, y, z)) = if ((null y && null z) || checkform y z) then True else False

-- | Check if a given sequent is a closed one, that is, there are corresponding formulas within in.
closedseq :: CanSeq -> Bool
closedseq (Can (x, y, z)) = checkform x x || checkform y z



-- | Apply a given rule to a singular sequent depending on the formulas it is comprised of.
applyrule :: CanSeq -> [CanSeq]
applyrule (Can (x, y, z))
		| atomicseq (Can (x, y, z))  = [Can (x, y, z)]
		| not (null z) 		   = ruleBeta (Can (x, y, z))
		| otherwise 		   = ruleAlpha (Can (x, y, z))

-- | Applying function applyrule on a given hypersequent.
applyrule1 :: [CanSeq] -> [CanSeq]
applyrule1 (x:xs) = if atomicseq x then (x:applyrule1 xs) else (applyrule x) ++ xs
applyrule1 [] = []

-- | Build a tree using rule applying rules to a hypersequent.
buildtree :: CanSeq -> Tree [CanSeq]
buildtree x = Node [x] [Node (applyrule x) []]

-- | Builds a tree for an argument of a tree type.
prooftree :: Tree [CanSeq] -> Tree [CanSeq]
prooftree (Node x [])
    	| all atomicseq x = (Node x [])
        | not (all atomicseq x) = (Node x [(Node (applyrule1 x) [])])
prooftree (Node x xs) = Node x (map prooftree xs)

-- | Check if a tree is an atomic one -- all of the leaves are atomic.
atom_tree :: Foldable t => Tree (t CanSeq) -> Bool
atom_tree (Node x []) = if all atomicseq x then True else False
atom_tree (Node x [z]) = atom_tree z

-- | Build a derivation for a hypersequent.
derivation :: [CanSeq] -> Tree [CanSeq]
derivation x = until (atom_tree) prooftree (Node x [])

-- | Using an above function derivation, derivation tree is returned for a singular formula.
derivationF :: For -> Tree [CanSeq]
derivationF x = derivation [sortCan x]

-- | Check if a tree is a proof.
istaut :: Tree [CanSeq] -> Bool
istaut (Node x []) = and [closedseq y | y <- x]
istaut (Node x rest) = and $ map istaut rest

-- | Check if a formula is a tautology.
isTaut1 :: For -> Bool
isTaut1 for = istaut $ derivationF for


-- | Number of sequents in final hypersequent.
numOfSeq :: Tree [CanSeq] -> Int
numOfSeq (Node x []) = length x
numOfSeq (Node x rest) = numOfSeq (head rest)

-- | Length of the derivation.
lenOfCanTree :: Tree [CanSeq] -> Int
lenOfCanTree (Node x []) = 1
lenOfCanTree (Node x rest) = 1 + lenOfCanTree (head rest)

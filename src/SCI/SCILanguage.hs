module SCILanguage where 

import Data.Tree
--import Language

-- | Inductive definition of Formula type
data For =   Verum
            | V Int
            | N For
            | For `E` For
            | For `I` For
            | For `A` For
            | For `D` For 
            | For `Id` For deriving(Eq,Read,Show,Ord)

newtype SciSeq =  Sci ([For], [For], [For]) deriving(Eq,Read,Show,Ord)


beta :: For -> Bool
beta x = case x of
 D _ _ -> True
 I _ _ -> True
 N (A _ _) -> True
 N (E _ _) -> True
 _     -> False

alpha :: For -> Bool
alpha x = case x of
 A _ _ -> True
 N (D _ _) -> True
 N (I _ _) -> True
 E _ _ -> True
 N (N _) -> True
 N (Id _ _) -> True
 _     -> False

var :: For -> Bool
var x = case x of
    V _ -> True
    N (V _) -> True
    Id _ _ -> True
    _ -> False

insertSci :: For -> SciSeq -> SciSeq
insertSci f (Sci (xs, ys, zs))
 | var f = Sci (f:xs, ys, zs)
 | beta f = Sci (xs, f:ys, zs)
 | otherwise = Sci (xs, ys, f:zs)

branchingSci :: SciSeq -> [SciSeq]
branchingSci (Sci (xs, y:ys, zs)) = case y of
    D n m -> [(insertSci n (Sci (xs,ys, zs))), (insertSci m (Sci (xs, ys, zs)))]
    N (A n m) -> [(insertSci (N n) (Sci (xs,ys, zs))), (insertSci (N m) (Sci (xs, ys, zs)))]
    I n m -> [insertSci (N n) (Sci (xs, ys, zs)), insertSci (m) (Sci (xs, ys, zs))]

nonbranchingSci :: SciSeq -> [SciSeq]
nonbranchingSci (Sci (xs, ys, z:zs)) = case z of
    A n m -> [insertSci m (insertSci n (Sci (xs,ys, zs)))]
    N (I n m) -> [insertSci (N m) (insertSci n (Sci (xs,ys, zs)))]
    N (D n m) -> [insertSci (N m) (insertSci (N n) (Sci (xs,ys, zs)))]
    N (N n) -> [insertSci n (Sci (xs, ys, zs))]


seqtolist :: SciSeq -> [For]
seqtolist (Sci (x, y, z)) = x ++ y ++ z

seqsum :: SciSeq -> Int
seqsum x = length (seqtolist x)

-- | Check if a given formula is an element of a sequent.
elemseq :: For -> SciSeq -> Bool
elemseq x (Sci (xs, ys, zs)) = elem x xs || elem x ys || elem x zs

complex :: For -> Int
complex x = case x of
    V _     -> 1
    N w     -> 1 + complex w
    I w y   -> 1 + complex w + complex y
    A w y   -> 1 + complex w + complex y
    D w y   -> 1 + complex w + complex y
    Id w y  -> 1 + complex w + complex y

idRule1 :: Int -> SciSeq -> SciSeq
idRule1 a (Sci (xs,ys,zs)) = (Sci (xs,ys,zs++w))
        where
         w = [N (Id x x)| x <- zs, (complex (Id x x) < a), not (elemseq N (Id x x))]

idRule2 :: SciSeq -> [SciSeq]
idRule2 (Sci (xs, ys, z:zs)) = case z of
    N (Id n m) -> [insertSci (N (I n m)) (Sci (xs,ys,zs)), not (elemseq N (I n m))]

idRule3 :: Int -> SciSeq -> SciSeq
idRule3 a (Sci (xs,ys,zs)) = (Sci (xs,ys,zs++w))
     where 
      w = [(N (Id (w x n) (w y m))) | (N (Id x y)) <- zs, (N (Id n m)) <- zs, w <- [A, D, I, Id], (complex (N (Id (w x n) (w y m))) < a), not (elemseq (N (Id (w x n) (w y m))))] 

ifAtomic :: SciSeq -> Bool
ifAtomic (Sci (x,y,z)) = if (null y && null z) then True else False
        
applyRule :: Int -> SciSeq -> [SciSeq]
applyRule a (Sci (x,y,z))
   | ifAtomic (Sci (x,y,z)) = s [Sci (x,y,z)]
   | not (null z)          = s (nonbranchingSci (Sci (x,y,z)))
   | otherwise             = s (branchingSci (Sci (x,y,z)))
      where
          s = \w -> map idRule2 (map (idRule3 a) (map (idRule1 a) w))
      

k = [Sci ([],[],[N (V 1 `Id` V 2),N ((V 1 `A` V 1) `Id` (V 2 `A` V 2)),N ((V 1 `D` V 1) `Id` (V 2 `D` V 2)),N ((V 1 `I` V 1) `Id` (V 2 `I` V 2)),N ((V 1 `Id` V 1) `Id` (V 2 `Id` V 2)),N (V 1 `Id` V 2) `Id` N (V 1 `Id` V 2),N ((V 1 `A` V 1) `Id` (V 2 `A` V 2)) `Id` N ((V 1 `A` V 1) `Id` (V 2 `A` V 2)),N ((V 1 `D` V 1) `Id` (V 2 `D` V 2)) `Id` N ((V 1 `D` V 1) `Id` (V 2 `D` V 2)),N ((V 1 `I` V 1) `Id` (V 2 `I` V 2)) `Id` N ((V 1 `I` V 1) `Id` (V 2 `I` V 2)),N ((V 1 `Id` V 1) `Id` (V 2 `Id` V 2)) `Id` N ((V 1 `Id` V 1) `Id` (V 2 `Id` V 2))]),Sci ([],[V 2 `I` V 1],[])]


-- | Applying function ke_applyRuleSeq on a given hypersequent.
applyRuleH  :: Int -> [SciSeq] -> [SciSeq]
applyRuleH a (x:xs) 
  | ifAtomic x = (x:applyRuleH a xs)
  | otherwise  = (applyRule a x) ++ (applyRuleH a xs)
applyRuleH a [] = []

-- | Build a tree using rule applying rules to a hypersequent.
buildScitree :: Int -> SciSeq -> Tree [SciSeq]
buildScitree a x = Node [x] [Node (applyRule a x) []]

ww = (Node {rootLabel = [Sci ([],[(V 1 `Id` V 2) `I` (V 2 `I` V 1)],[])], subForest = [Node {rootLabel = [Sci ([],[],[N (V 1 `Id` V 2),N ((V 1 `A` V 1) `Id` (V 2 `A` V 2)),N ((V 1 `D` V 1) `Id` (V 2 `D` V 2)),N ((V 1 `I` V 1) `Id` (V 2 `I` V 2)),N ((V 1 `Id` V 1) `Id` (V 2 `Id` V 2)),N (V 1 `Id` V 2) `Id` N (V 1 `Id` V 2),N ((V 1 `A` V 1) `Id` (V 2 `A` V 2)) `Id` N ((V 1 `A` V 1) `Id` (V 2 `A` V 2)),N ((V 1 `D` V 1) `Id` (V 2 `D` V 2)) `Id` N ((V 1 `D` V 1) `Id` (V 2 `D` V 2)),N ((V 1 `I` V 1) `Id` (V 2 `I` V 2)) `Id` N ((V 1 `I` V 1) `Id` (V 2 `I` V 2)),N ((V 1 `Id` V 1) `Id` (V 2 `Id` V 2)) `Id` N ((V 1 `Id` V 1) `Id` (V 2 `Id` V 2))]),Sci ([],[V 2 `I` V 1],[])], subForest = []}]})
{-
Node {rootLabel = [Sci ([],[(V 1 `I` V 2) `I` ((V 1 `Id` V 4) `Id` (V 2 `Id` V 5))],[])], 
subForest = [Node {rootLabel = [Sci ([],[],[N (V 1 `I` V 2)]),Sci ([(V 1 `Id` V 4) `Id` (V 2 `Id` V 5)],[],[])], 
subForest = [Node {rootLabel = [Sci ([N (V 2),V 1],[],[]),Sci ([(V 1 `Id` V 4) `Id` (V 2 `Id` V 5)],[],[])], 
subForest = []}]}]}
-}


ifAtomicTree :: Foldable t => Tree (t SciSeq) -> Bool
ifAtomicTree (Node x []) = if all ifAtomic x then True else False
ifAtomicTree (Node x [z]) = ifAtomicTree z

sciprooftree :: Int -> Tree [SciSeq] -> Tree [SciSeq]
sciprooftree a (Node x [])
 | all ifAtomic x = (Node x [])
 | otherwise = (Node x [(Node (applyRuleH a x) [])])
sciprooftree a (Node x xs) = Node x (map proof xs)
          where
              proof = \z -> sciprooftree a z

h1 = (Node {rootLabel = [Sci ([],[(V 1 `Id` V 2) `I` (((V 1 `Id` V 3) `Id` (V 4 `Id` V 5)) `I` ((V 2 `Id` V 3) `Id` (V 4 `Id` V 5)))],[])], subForest = [Node {rootLabel = [Sci ([],[],[N (V 1 `Id` V 2)]),Sci 
([],[((V 1 `Id` V 3) `Id` (V 4 `Id` V 5)) `I` ((V 2 `Id` V 3) `Id` (V 4 `Id` V 5))],[])], subForest = []}]})

h2 = (Node {rootLabel = [Sci ([],[(V 1 `Id` V 2) `I` (((V 1 `Id` V 3) `Id` (V 4 `Id` V 5)) `I` ((V 2 `Id` V 3) `Id` (V 4 `Id` V 5)))],[])], subForest = [Node {rootLabel = [Sci ([],[],[N (V 1 `Id` V 2)]),Sci 
([],[((V 1 `Id` V 3) `Id` (V 4 `Id` V 5)) `I` ((V 2 `Id` V 3) `Id` (V 4 `Id` V 5))],[])], subForest = [Node {rootLabel = [Sci ([],[],[N (V 1 `I` V 2)]),Sci ([],[],[N ((V 1 `Id` V 3) `Id` (V 4 `Id` V 5))]),Sci ([(V 2 `Id` V 3) `Id` (V 4 `Id` V 5)],[],[])], subForest = []}]}]})
h3 = (Node {rootLabel = [Sci ([],[(N (V 1) `A` (V 1 `Id` V 2)) `I` N (V 2)],[])], subForest = [Node {rootLabel = [Sci ([],[N (N (V 1) `A` (V 1 `Id` V 2))],[]),Sci ([N (V 2)],[],[])], subForest = [Node {rootLabel = [Sci ([],[],[N (N (V 1)),N (N (V 1)) `Id` N (N (V 1))]),Sci ([],[],[N (V 1 `Id` V 2),N ((V 1 `A` V 1) `Id` (V 2 `A` V 2)),N ((V 1 `D` V 1) `Id` (V 2 `D` V 2)),N ((V 1 `I` V 1) `Id` (V 2 `I` V 2)),N ((V 1 `Id` V 1) `Id` (V 2 `Id` V 2)),N (V 1 `Id` V 2) `Id` N (V 1 `Id` V 2),N ((V 1 `A` V 1) `Id` (V 2 `A` V 2)) `Id` N ((V 1 `A` V 1) `Id` (V 2 `A` V 2)),N ((V 1 `D` V 1) `Id` (V 2 `D` V 2)) `Id` 
N ((V 1 `D` V 1) `Id` (V 2 `D` V 2)),N ((V 1 `I` V 1) `Id` (V 2 `I` V 2)) `Id` N ((V 1 `I` V 1) `Id` (V 2 `I` V 2)),N ((V 1 `Id` V 1) `Id` (V 2 `Id` V 2)) `Id` N ((V 1 `Id` V 1) `Id` (V 2 `Id` V 2))]),Sci ([N (V 2)],[],[])], subForest = []}]}]})
-- | Build a derivation for a hypersequent.
derivation :: Int -> [SciSeq] -> Tree [SciSeq]
derivation a x = until (ifAtomicTree) proof (Node x [])
                where
                    proof = \z -> sciprooftree a z

-- sorts elements of a given list to one of three
-- depending on its category (variable, alpha, beta)
sortList :: [For] -> ([For],[For],[For]) -> ([For],[For],[For])
sortList [] (a,b,c) = (a,b,c)
sortList (x:xs) (a,b,c)
    | var x   = sortList xs (x:a,b,c)
    | alpha x = sortList xs (a,b,x:c)
    | beta x  = sortList xs (a,x:b,c)

-- similar as the function above, but with singular formula
sortFormula :: For -> ([For],[For],[For])
sortFormula x = sortList [x] ([],[],[])

-- | Using an above function derivation, derivation tree is returned for a singular formula.
derivationF :: Int -> For -> Tree [SciSeq]
derivationF a x = derivation a [Sci $ sortFormula x]

--from AuxiliaryFormulas
checkform :: [For] -> [For] -> Bool
checkform [] ys = False
checkform (x:xs) ys = case x of
    N z -> z `elem` ys || checkform xs ys
    z -> (N z) `elem` ys || checkform xs ys

-- | Check if Sci sequent is closed (complementary formulas)
closedhs :: SciSeq -> Bool
closedhs (Sci (x,y,z)) = checkform x x

{-
Node {rootLabel = [Sci ([],[(V 1 `Id` V 2) `I` (N (V 1) `Id` N (V 2))],[])], subForest = [Node {rootLabel = [Sci ([],[],[N (V 1 `Id` V 2)]),Sci ([N (V 1) `Id` N (V 2)],[],[])], 
subForest = [Node {rootLabel = [Sci ([],[],[N (V 1 `I` V 2)]),Sci ([N (V 1) `Id` N (V 2)],[],[])], 
subForest = [Node {rootLabel = [Sci ([N (V 2),V 1],[],[]),Sci ([N (V 1) `Id` N (V 2)],[],[])], 
subForest = []}]}]}]}
-}

-- | Check if a tree is atomic, that is, all of the leaves are atomic.
closed_tree :: Tree [SciSeq] -> Bool
closed_tree (Node x []) = if any closedhs x then True else False
closed_tree (Node x [z]) = closed_tree z

isTaut :: Int -> For -> Bool
isTaut a x = closed_tree (derivationF a x)

{-
Aksjomaty:
A = A
(A = B) (¬A = ¬B)
(A = B) → (B → A)
((A = C) & (B = D)) → ((A o B) = (C o D)), gdzie o to dowolny spójnik
Inne tezy:
(A = B) → (B = A)
((A = B) & A) → B
(A = B) →((B = C) → (A = C))
Formuły, które nie wyglądają na tautologie, ale nimi są:
(((B = A) → (A → C)) = ((A → (A ↔ A)) = A)) → (((C ∧ A) ↔ (A = A)) ∨ ((A ∧ A) ∨ ¬B))
(A = B)→((X → (Y → (Z → A))) → (X → (Y → (Z → B))))

(p=q) -> [((p = r) = (s = t)) -> ((q = r) = (s = t))] oraz (p=q) -> [((s = p) -> (p = r)) -> ((s = q) = (q = r))]

((V 1 `Id` V 2) `I` (((V 1 `Id` V 3) `Id` (V 4 `Id` V 5)) `I` ((V 2 `Id` V 3) `Id` (V 4 `Id` V 5))))

([],[(V 1 `Id` V 2) `I` (((V 1 `Id` V 3) `Id` (V 4 `Id` V 5)) `I` ((V 2 `Id` V 3) `Id` (V 4 `Id` V 5)))],[])
-}

{-
Node {rootLabel = [Sci ([],[(V 1 `Id` V 2) `I` (((V 1 `Id` V 3) `Id` (V 4 `Id` V 5)) `I` ((V 2 `Id` V 3) `Id` (V 4 `Id` V 5)))],[])], 
subForest = [Node {rootLabel = [Sci ([],[],[N (V 1 `Id` V 2)]),Sci 
([],[],[])], 
subForest = [Node {rootLabel = [Sci ([],[],[N (V 1 `I` V 2)]),Sci ([],[],[])], subForest = [Node {rootLabel = [Sci ([N (V 2),V 1],[],[]),Sci ([],[],[])], subForest = []}]}]}]}

buildScitree
Node {rootLabel = [Sci ([],[(V 1 `Id` V 2) `I` (((V 1 `Id` V 3) `Id` (V 4 `Id` V 5)) `I` ((V 2 `Id` V 3) `Id` (V 4 `Id` V 5)))],[])], subForest = [Node {rootLabel = [Sci ([],[],[N (V 1 `Id` V 2)]),Sci 
([],[((V 1 `Id` V 3) `Id` (V 4 `Id` V 5)) `I` ((V 2 `Id` V 3) `Id` (V 4 `Id` V 5))],[])], subForest = []}]}

sciprooftree
Node {rootLabel = [Sci ([],[(V 1 `Id` V 2) `I` (((V 1 `Id` V 3) `Id` (V 4 `Id` V 5)) `I` ((V 2 `Id` V 3) `Id` (V 4 `Id` V 5)))],[])], 
subForest = [Node {rootLabel = [Sci ([],[],[N (V 1 `Id` V 2)]),Sci ([],[((V 1 `Id` V 3) `Id` (V 4 `Id` V 5)) `I` ((V 2 `Id` V 3) `Id` (V 4 `Id` V 5))],[])], 
subForest = [Node {rootLabel = [Sci ([],[],[N (V 1 `I` V 2)]),Sci ([],[],[N ((V 1 `Id` V 3) `Id` (V 4 `Id` V 5))]),Sci ([(V 2 `Id` V 3) `Id` (V 4 `Id` V 5)],[],[])], 
subForest = []}]}]}
-}


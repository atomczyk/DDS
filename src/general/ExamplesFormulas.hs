module ExamplesFormulas where

import Language
import AuxiliaryTrees
import AuxiliaryFormulas
import CutRule
import CanonicalTrees
import OptimizedTrees
import FlexibleTrees
--import FormulaGenerators
import PrintingTrees
import Data.Tree
import Data.List
import Text.PrettyPrint
import Canonical
import Dual
import AuxiliaryFunctions
import Data.Set hiding (filter, map, null)

f1 = I (A (I (N (V 1)) (D (V 2) (V 1))) (D (V 1) (V 2))) (N (V 2))
-- p v ~p
f2 = D (V 1) (N (V 1))
-- (p v ¬q) → (r & q)
f3 = I (D (V 1) (N (V 3))) (A (V 10) (V 3))
-- absrd
f4 = I (I (V 1) (V 2)) (I (I (V 1) (N (V 2))) (N (V 1) ))

f5 = I (D (N (V 1)) (V 2)) (V 3)

f6 = (A (V 1) (V 2)) `D` N (A (V 1) (V 2))

-- (p → q) $ (p & r → q & r)
f7 = A (I (V 1) (V 2)) (I (A (V 1) (V 3)) (A (V 2) (V 3)))
-- p v (q v r) → (p v q) v r
f8 = I (D (V 1) (D (V 2) (V 3))) (D (D (V 1) (V 2)) (V 3))
-- (p → q) → (p v r → q v r)
f9 = I (I (V 1) (V 2)) (I (D (V 1) (V 3)) (D (V 2) (V 3)))
-- (p & q → r) → (p → (q → r))
f10 = I (I (A (V 1) (V 2)) (V 3)) (I (V 1) (I (V 2) (V 3)))
-- (p <=> q) → ((r <=> s) → (p & r <=> q & s))
f11  = I (E (V 1) (V 2)) (I (E (V 3) (V 4)) (E (A (V 1) (V 3)) (A (V 2) (V 4))))


fail = N (N (N (N (I (V 1) (N (V 2))))))
failtree = Node {rootLabel = ([Dual ([],[],[N (N (N (N (I (V 1) (N (V 2))))))])],[]),
subForest = [Node {rootLabel = ([Dual ([],[],[N (N (I (V 1) (N (V 2))))])],[]),
subForest = [Node {rootLabel = ([Dual ([],[I (V 1) (N (V 2))],[])],[]),
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[])],[]), subForest = []}]}]}]}


fail2 = N (N (N (I (I (V 1) (V 1)) (V 1))))
fail2tree = Node {rootLabel = ([Dual ([],[],[N (N (N (I (I (V 1) (V 1)) (V 1))))])],[]),
subForest = [Node {rootLabel = ([Dual ([],[],[N (I (I (V 1) (V 1)) (V 1))])],[]),
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[I (V 1) (V 1)],[])],[]),
subForest = [Node {rootLabel = ([Dual ([N (V 1),N (V 1)],[],[]),Dual ([V 1,N (V 1)],[],[])],[]), subForest = []}]}]}]}

res = resolution_poss2 (Node {rootLabel = ([Dual ([V 2,N (V 1)],[],[]), Dual ([V 1,V 2],[],[])], [(Dual ([V 1,V 2],[],[]), Dual ([V 2,N (V 1)],[],[]))]), subForest = []})

testtree = Node {rootLabel = ([Dual ([V 2,N (V 3)],[],[]),Dual ([N (V 1),V 3,V 2],[],[]),Dual ([N (V 1),N (V 1)],[],[]),Dual ([V 2],[],[]),Dual ([N (V 1),V 2],[],[]),Dual ([V 2,N (V 1)],[],[]),Dual ([V 2,V 3,V 2],[],[]),Dual ([N (V 1),N (V 3)],[],[])],[(Dual ([V 2,V 3,V 2],[],[]),Dual ([N (V 1),N (V 3)],[],[])),(Dual ([N (V 1),N (V 3)],[],[]),Dual ([N (V 1),V 3,V 2],[],[])),(Dual ([N (V 1),V 3,V 2],[],[]),Dual ([V 2,N (V 3)],[],[])),(Dual ([V 2,N (V 3)],[],[]),Dual ([V 2,V 3,V 2],[],[]))]), subForest = [Node {rootLabel = ([Dual ([V 2,N (V 1)],[],[]),Dual ([N (V 1),V 2],[],[]),Dual ([V 2],[],[]),Dual ([N (V 1),N (V 1)],[],[]),Dual ([V 2,N (V 3)],[],[]),Dual ([N (V 1),V 3,V 2],[],[]),Dual ([V 2,V 3,V 2],[],[]),Dual ([N (V 1),N (V 3)],[],[])],[(Dual ([V 2,V 3,V 2],[],[]),Dual ([N (V 1),N (V 3)],[],[])),(Dual ([N (V 1),N (V 3)],[],[]),Dual ([N (V 1),V 3,V 2],[],[])),(Dual ([N (V 1),V 3,V 2],[],[]),Dual ([V 2,N (V 3)],[],[])),(Dual ([V 2,N (V 3)],[],[]),Dual ([V 2,V 3,V 2],[],[]))]), subForest = []}]}

-- ~(q -> ((p -> p) -> q)) = q ^ ~((p -> p) -> q)) = q ^ (p->p) ^ ~q
a = N (I (V 2) (I (I (V 1) (V 1)) (V 2)))
a_con = (V 2) `A` (((V 1) `I` (V 1)) `A` (N (V 2)))
a_con1 = N (a_con)
a1 = ([Dual ([V 1,N (V 2),V 2],[],[]),Dual ([N (V 1),V 2,N (V 2)],[],[]),Dual ([V 2,V 1,N (V 2)],[],[]),Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])],[(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])),(Dual ([N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[])),(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[]))])
a2 = ([Dual ([V 2,V 1,N (V 2)],[],[]),Dual ([V 1,N (V 2),V 2],[],[]),Dual ([N (V 1),V 2,N (V 2)],[],[]),Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])],[(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])),(Dual ([N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[])),(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[]))])
a3 = ([Dual ([N (V 1),V 2,N (V 2)],[],[]),Dual ([V 2,V 1,N (V 2)],[],[]),Dual ([V 1,N (V 2),V 2],[],[]),Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])],[(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])),(Dual ([N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[])),(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[]))])
a4 = ([Dual ([V 1,N (V 2),V 2],[],[]),Dual ([N (V 1),V 2,N (V 2)],[],[]),Dual ([V 2,V 1,N (V 2)],[],[]),Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])],[(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])),(Dual ([N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[])),(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[]))])
a5 = ([Dual ([V 2,V 1,N (V 2)],[],[]),Dual ([V 1,N (V 2),V 2],[],[]),Dual ([N (V 1),V 2,N (V 2)],[],[]),Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])],[(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])),(Dual ([N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[])),(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[]))])
a6 = ([Dual ([N (V 1),V 2,N (V 2)],[],[]),Dual ([V 2,V 1,N (V 2)],[],[]),Dual ([V 1,N (V 2),V 2],[],[]),Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])],[(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])),(Dual ([N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[])),(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[]))])




a7 = fromList (fst a5)
a8 = fromList (fst a6)

x = Node {rootLabel = ([Dual ([],[],[N (I (V 2) (I (I (V 1) (V 1)) (V 2)))])],[]),
subForest = [Node {rootLabel = ([Dual ([V 2],[],[N (I (I (V 1) (V 1)) (V 2))])],[]),
subForest = [Node {rootLabel = ([Dual ([N (V 2),V 2],[I (V 1) (V 1)],[])],[]),
subForest = [Node {rootLabel = ([Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[])],[]),
subForest = [Node {rootLabel = ([Dual ([N (V 2),V 2],[],[]),Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[])],[(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[]))]),
subForest = [Node {rootLabel = ([Dual ([V 2,V 1,N (V 2)],[],[]),Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[])],[(Dual ([N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[])),(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[]))]),
subForest = [Node {rootLabel = ([Dual ([N (V 1),V 2,N (V 2)],[],[]),Dual ([V 2,V 1,N (V 2)],[],[]),Dual ([V 1,N (V 2),V 2],[],[]),Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])],[(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])),(Dual ([N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[])),(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[]))]),
 subForest = [Node {rootLabel = ([Dual ([V 1,N (V 2),V 2],[],[]),Dual ([N (V 1),V 2,N (V 2)],[],[]),Dual ([V 2,V 1,N (V 2)],[],[]),Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])],
 [(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([N (V 2),V 2],[],[])),(Dual ([N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[])),(Dual ([N (V 1),N (V 2),V 2],[],[]),Dual ([V 1,N (V 2),V 2],[],[]))]), subForest = []}]}]}]}]}]}]}]}

testtree2 = Node ([], []) [Node ([],[]) []]

{-
f2 = D (V 1) (N (V 1))

fx = D (A (V 1) (V 2)) (N (V 2))
-- (p -> q) & ((p & r) -> (q & r))
f7 = A (I (V 1) (V 2)) (I (A (V 1) (V 3)) (A (V 2) (V 3)))

f1 = I (A (I (N (V 1)) (D (V 2) (V 1))) (D (V 1) (V 2))) (N (V 2))
-- p v ~p
f2x = A (V 1) (N (V 10))
f2y = N (A (I (V 1) (N (V 1))) (V 2))
-- (p v ¬q) → (r & q)
f3 = I (D (V 1) (N (V 3))) (A (V 10) (V 3))
-- absrd
f4 = I (I (V 1) (V 2)) (I (I (V 1) (N (V 2))) (N (V 1) ))

f5 = I (D (N (V 1)) (V 2)) (V 3)

f6 = (A (V 1) (V 2)) `D` N (A (V 1) (V 2))

-- (p → q) -> (p & r → q & r)
f7t = I (I (V 1) (V 2)) (I (A (V 1) (V 3)) (A (V 2) (V 3)))
-- p v (q v r) → (p v q) v r
f8 = I (D (V 1) (D (V 2) (V 3))) (D (D (V 1) (V 2)) (V 3))
-- (p → q) → (p v r → q v r)
f9 = I (I (V 1) (V 2)) (I (D (V 1) (V 3)) (D (V 2) (V 3)))
-- (p & q → r) → (p → (q → r))
f10 = I (I (A (V 1) (V 2)) (V 3)) (I (V 1) (I (V 2) (V 3)))
-- (p <=> q) → ((r <=> s) → (p & r <=> q & s))
f11  = I (E (V 1) (V 2)) (I (E (V 3) (V 4)) (E (A (V 1) (V 3)) (A (V 2) (V 4))))
-}
isTaut :: For -> Bool
isTaut x = closed_tree (proofDualtestx x)


---amelia
for1 = (((V 1) `I` (V 2)) `A` (V 1)) `I` (V 2)
for1neg = N ((((V 1) `I` (V 2)) `A` (V 1)) `I` (V 2))
for2 = (((V 1) `I` (V 2)) `A` (V 2)) `I` (V 1)
for2neg = N ((((V 1) `I` (V 2)) `A` (V 2)) `I` (V 1))
for3 = (((V 1) `I` (V 2)) `A` ((V 2) `I` (V 3))) `I` ((V 1) `I` (V 3))
for3neg = N ((((V 1) `I` (V 2)) `A` ((V 2) `I` (V 3))) `I` ((V 1) `I` (V 3)))
for4 =  ((((V 1) `I` (V 3)) `A` ((V 2) `I` (V 3))) `A` ((V 1) `D` (V 2))) `I` (V 3)
for4neg =  N (((((V 1) `I` (V 3)) `A` ((V 2) `I` (V 3))) `A` ((V 1) `D` (V 2))) `I` (V 3))


{-
Node {rootLabel = ([Dual ([],[],[A (I (V 1) (V 2)) (I (A (V 1) (V 3)) (A (V 2) (V 3)))])],[]), subForest = [Node {rootLabel = ([Dual ([],[I (A (V 1) (V 3)) (A (V 2) (V 3)),I (V 1) (V 2)],[])],[]), subForest = [Node {rootLabel = ([Dual ([],[N (A (V 1) (V 3)),I (V 1) (V 2)],[]),Dual ([],[I (V 1) (V 2)],[A (V 2) (V 3)])],[]), subForest = [Node {rootLabel = ([Dual ([N (V 1)],[I (V 1) (V 2)],[]),Dual ([N (V 3)],[I (V 1) (V 2)],[]),Dual ([],[I (V 1) (V 2)],[A (V 2) (V 3)])],[]), subForest = [Node {rootLabel = ([Dual ([N (V 1),N (V 1)],[],[]),Dual ([V 2,N (V 1)],[],[]),Dual ([N (V 3)],[I (V 1) (V 2)],[]),Dual ([],[I (V 1) (V 2)],[A (V 2) (V 3)])],[]), subForest = [Node {rootLabel = ([Dual ([N (V 1),N (V 1)],[],[]),Dual ([V 2,N (V 1)],[],[]),Dual ([N (V 1),N (V 3)],[],[]),Dual ([V 2,N (V 3)],[],[]),Dual ([],[I (V 1) (V 2)],[A (V 2) (V 3)])],[]), subForest = [Node {rootLabel = ([Dual ([N (V 1),N (V 1)],[],[]),Dual ([V 2,N (V 1)],[],[]),Dual ([N (V 1),N (V 3)],[],[]),Dual ([V 2,N (V 3)],[],[]),Dual ([V 3,V 2],[I (V 1) (V 2)],[])],[]),


 subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([V 2,N (V 1)],[],[]),Dual ([N (V 1),N (V 3)],[],[]),Dual ([V 2,N (V 3)],[],[]),Dual ([N (V 1),V 3,V 2],[],[]),Dual ([V 3,V 2],[],[])],[]), subForest = []}]}]}]}]}]}]}]


subForest = [Node {rootLabel = ([Dual ([N (V 1),V 2],[],[]),Dual ([N (V 1)],[],[]),Dual ([V 2,N (V 1)],[],[]),Dual ([V 3,V 2],[],[]),Dual ([V 2,N (V 3)],[],[]),Dual ([N (V 1),N (V 3)],[],[]),Dual ([N (V 1),V 3,V 2],[],[])],[]), subForest = []}]}
-}
{-
-- as above, but starting with a hypersequent
derdual :: [DualSeq] -> Tree [DualSeq]
derdual (x:xs) = until (fun3) prooftree2 (Node (x:xs) [])

-- as above, but starting with a hypersequent
derdualx :: [DualSeq] -> Tree [DualSeq]
derdualx (x:xs) = until (fun3x) prooftree2x (Node (x:xs) [])

-- searching for the first hyperseqeunt fulfilling criteria for resolution
fun3 :: Tree [DualSeq] -> Bool
fun3 (Node (x:xs) []) = fun2 x xs == (x:xs)
fun3 (Node (x:xs) [z]) = fun3 z

-- searching for the first hyperseqeunt fulfilling criteria for resolution
fun3x :: Tree [DualSeq] -> Bool
fun3x (Node xs []) = fun2x (pairs xs) == xs
fun3x (Node xs [z]) = fun3x z
-}

-- Complete derivation. Logical rules are applied first. Then resolution takes over.
{--- stops after applying the first logical rule
proofDual :: For -> Tree [DualSeq]
proofDual for = prooftree2 (dualprooftree root)
  where
    start = insertDual for (Dual ([],[],[]))
    root  = (Node [start] [])

-- does not stop for now, most likely due to some issues with resolution functions (without x)
proofDualtest :: For -> Tree [DualSeq]
proofDualtest x = until (der_cond) prooftree2 (derivationF x) -- looping

proofDualtest x = until (closed_tree) prooftree2 (derivationF x) --looping
proofDualtest x = prooftree2 (prooftree2 (prooftree2 (prooftree2 (derivationF x)))) -- works

-- two conditions: either hypersequent is closed or resolution is not possible
der_cond :: Tree [DualSeq] -> Bool
der_cond x = if (closed_tree x || not (resolution_poss x)) then True else False
-}

{-
--modified version of the above, different functions
-- does not stop for now, most likely due to some issues with resolution functions
proofDualtestx :: For -> Tree ([DualSeq], [(DualSeq, DualSeq)])
proofDualtestx x = until (der_condx) prooftree2x (derivationF x)

proofDualtestxx :: For -> Tree ([DualSeq], [(DualSeq, DualSeq)])
proofDualtestxx x = until (der_condx) prooftree2x (derivationFrmdups x)

-- two conditions: either hypersequent is closed or resolution is not possible
der_condx :: Tree ([DualSeq], [(DualSeq, DualSeq)]) -> Bool
der_condx x = if (closed_tree x || not (resolution_poss2 x)) then True else False


-----------------EXAMPLES---------------------


f1 = I (A (I (N (V 1)) (D (V 2) (V 1))) (D (V 1) (V 2))) (N (V 2))
-- p v ~p
f2 = D (V 1) (N (V 1))
f2x = A (V 1) (N (V 10))
f2y = N (A (I (V 1) (N (V 1))) (V 2))
-- (p v ¬q) → (r & q)
f3 = I (D (V 1) (N (V 3))) (A (V 10) (V 3))
-- absrd
f4 = I (I (V 1) (V 2)) (I (I (V 1) (N (V 2))) (N (V 1) ))

f5 = I (D (N (V 1)) (V 2)) (V 3)

f6 = (A (V 1) (V 2)) `D` N (A (V 1) (V 2))

-- (p → q) & (p & r → q & r)
f7 = I (I (V 1) (V 2)) (I (A (V 1) (V 3)) (A (V 2) (V 3)))
-- p v (q v r) → (p v q) v r
f8 = I (D (V 1) (D (V 2) (V 3))) (D (D (V 1) (V 2)) (V 3))
-- (p → q) → (p v r → q v r)
f9 = I (I (V 1) (V 2)) (I (D (V 1) (V 3)) (D (V 2) (V 3)))
-- (p & q → r) → (p → (q → r))
f10 = I (I (A (V 1) (V 2)) (V 3)) (I (V 1) (I (V 2) (V 3)))
-- (p <=> q) → ((r <=> s) → (p & r <=> q & s))
f11  = I (E (V 1) (V 2)) (I (E (V 3) (V 4)) (E (A (V 1) (V 3)) (A (V 2) (V 4))))



test1 = Dual ([(V 1), (V 2)], [], [])
test2 = Dual ([N (V 1), N (V 2), N (V 1)], [], [])
test3 = Dual ([], [], [N (D (V 1) (V 2))])
test4 = Dual ([], [D (V 1) (V 2)], [])
test5 = Dual ([V 1], [], [])
test6 = Dual ([V 2], [], [])
test7 = Dual ([N (V 1)], [], [])


--last working step in f7 derivation (using derivationF)
dual1 =  [Dual ([N (V 1),N (V 1)],[],[]),Dual ([V 2,N (V 1)],[],[]),Dual ([N (V 1),N (V 3)],[],[]),Dual ([V 2,N (V 3)],[],[]),Dual ([N (V 1),V 3,V 2],[],[]),Dual ([V 2,V 3,V 2],[],[])]
--derivationFrmdups
dual2 =  [Dual ([N (V 1)],[],[]),Dual ([V 2,N (V 1)],[],[]),Dual ([N (V 1),N (V 3)],[],[]),Dual ([V 2,N (V 3)],[],[]),Dual ([N (V 1),V 3,V 2],[],[]),Dual ([V 3,V 2],[],[])]
-}

module Dual3 where

import Language
import Data.Tree
import Sort
import Data.List
import Text.PrettyPrint
import Data.Set hiding (filter, map, null)
import AuxiliaryFunctions
import AuxiliaryFormulas


-- | Insert a singular formula in a proper list in a sequent, depending on its form.
insertDual :: For -> DualSeq -> DualSeq
insertDual f (Dual (xs, ys, zs))
 | var f  = Dual (f:xs, ys, zs)
 | beta f = Dual (xs, f:ys, zs)
 | otherwise = Dual (xs, ys, f:zs)


-- | Branching rules for logical connectives in dual calculus.
branchingDual :: DualSeq -> [DualSeq]
branchingDual (Dual (xs, y:ys, zs)) = case y of
    D n m -> [(insertDual n (Dual (xs,ys, zs))), (insertDual m (Dual (xs, ys, zs)))]
    N (A n m) -> [(insertDual (N n) (Dual (xs,ys, zs))), (insertDual (N m) (Dual (xs, ys, zs)))]
    I n m -> [insertDual (N n) (Dual (xs, ys, zs)), insertDual (m) (Dual (xs, ys, zs))]
    E n m -> [(insertDual (N m) (insertDual (N n) (Dual (xs,ys, zs)))), (insertDual n (insertDual m (Dual (xs, ys, zs))))]
    N (E n m) -> [(insertDual m (insertDual (N n) (Dual (xs,ys, zs)))), (insertDual n (insertDual (N m) (Dual (xs, ys, zs))))]
    --N (N n) -> [insertDual n (Dual (xs, ys, zs))]

-- | Non-branching rules for logical connectives in dual calculus.
nonbranchingDual :: DualSeq -> [DualSeq]
nonbranchingDual (Dual (xs, ys, z:zs)) = case z of
    A n m -> [insertDual m (insertDual n (Dual (xs,ys, zs)))]
    N (I n m) -> [insertDual (N m) (insertDual n (Dual (xs,ys, zs)))]
    N (D n m) -> [insertDual (N m) (insertDual (N n) (Dual (xs,ys, zs)))]
    N (N n) -> [insertDual n (Dual (xs, ys, zs))]
--    E n m -> [(insertDual (N m) (insertDual (N n) (Dual (xs,ys, zs)))), (insertDual n (insertDual m (Dual (xs, ys, zs))))]
--    N (E n m) -> [(insertDual m (insertDual (N n) (Dual (xs,ys, zs)))), (insertDual n (insertDual (N m) (Dual (xs, ys, zs))))]


-- | Checks if a given sequent is an atomic one, that is, there are only variables left. (|| checkform y z deleted)
atomicDualseq :: DualSeq -> Bool
atomicDualseq (Dual (x, y, z)) = if (null y && null z) then True else False

-- | Apply a given rule to a singular sequent depending on the formulas it is comprised of.
applyDualRules :: DualSeq -> [DualSeq]
applyDualRules (Dual (x, y, z))
 | atomicDualseq (Dual (x, y, z)) = [Dual (x, y, z)]
 | not (null z)                   = nonbranchingDual (Dual (x, y, z))
 | otherwise                      = branchingDual (Dual (x, y, z))


-- | Applying function applyDualRules on a given hypersequent.
applyDualrule1 :: [DualSeq] -> [DualSeq]
applyDualrule1 [] = []
applyDualrule1 (x:xs) = if atomicDualseq x then (x:applyDualrule1 xs) else (applyDualRules x) ++ xs


-- | Build a tree using rule applying rules to a hypersequent.
buildDualtree :: DualSeq -> Tree ([DualSeq], [(DualSeq, DualSeq)])
buildDualtree x = Node ([x], []) [Node ((applyDualRules x), []) []]

-- | Builds a tree for an argument of a tree type.
dualprooftree :: Tree ([DualSeq], [(DualSeq, DualSeq)]) -> Tree ([DualSeq], [(DualSeq, DualSeq)])
dualprooftree (Node (x, []) [])
 | all atomicDualseq x = (Node (x, []) [])
 | not (all atomicDualseq x) = (Node (x, []) [(Node ((applyDualrule1 x), []) [])])
dualprooftree (Node (x, us) xs) = Node (x, us) (map dualprooftree xs)


-- | Check if a tree is an atomic one -- all of the leaves are atomic.
atom_tree :: Foldable t => Tree (t DualSeq, b) -> Bool
atom_tree (Node (x, us) []) = if all atomicDualseq x then True else False
atom_tree (Node (x, us) [z]) = atom_tree z

-- | Build a derivation for a hypersequent.
derivation :: [DualSeq] -> Tree ([DualSeq], [(DualSeq, DualSeq)])
derivation x = until (atom_tree) dualprooftree (Node (x,[]) [])


-- | Using an above function derivation, derivation tree is returned for a singular formula.
derivationF :: For -> Tree ([DualSeq], [(DualSeq, DualSeq)])
derivationF x = derivation [sortDual x]

-- | Remove duplicated formulas from a sequent.
rmdupsvar :: DualSeq -> DualSeq
rmdupsvar (Dual (x, y, z)) = Dual (rmdups x, y, z)

-- | Remove duplicated formulas from all sequents in the leaves.
rmdupsleaves :: Tree ([DualSeq], [(DualSeq, DualSeq)]) -> Tree ([DualSeq], [(DualSeq, DualSeq)])
rmdupsleaves (Node (xs,[]) []) = Node ((map (\x -> rmdupsvar x) xs), []) []
rmdupsleaves (Node (x,[]) xs) = Node (x,[]) (map rmdupsleaves xs)

-- | Applying the function rmdupsleaves on a tree build from a singular formula.
derivationFrmdups :: For -> Tree ([DualSeq], [(DualSeq, DualSeq)])
derivationFrmdups x = rmdupsleaves (derivation [sortDual x])

-----------RESOLUTION----------------

-- | Return complementary formulas.
compl :: For -> For
compl x =  case x of
 N z -> z
 z -> N z

-- | Check if a given formula is an element of a sequent.
elemseq :: For -> DualSeq -> Bool
elemseq x (Dual (xs, ys, zs)) = elem x xs || elem x ys || elem x zs

-- | Return a list of complementary formulas for two given lists.
returncompl :: [For] -> [For] -> [For]
returncompl [] zs = []
returncompl xs [] = []
returncompl (x:xs) zs = if elem (compl x) zs then x:(compl x):[] else returncompl xs zs


-- | Check if dual hypersequent is closed -- all lists are empty.
closedhyp :: [DualSeq] -> Bool
closedhyp xs = any (\x -> x == Dual ([], [], [])) xs


-- | Check if a tree is atomic, that is, all of the leaves are atomic.
closed_tree :: Tree ([DualSeq], [(DualSeq, DualSeq)]) -> Bool
closed_tree (Node (x, us) []) = if closedhyp x then True else False
closed_tree (Node (x, us) [z]) = closed_tree z

-- | Can resolution functions be called: (1) are there complementary formulas for a given pair? (2) does this particular pair belong to the memory?
resolution_poss2 :: Tree ([DualSeq], [(DualSeq, DualSeq)]) -> Bool
resolution_poss2 (Node (xs, us) []) = if b /= []  then True else False
    where
        z = map (\n -> compllist (fst n) (snd n)) (pairs xs)
        m = pairs xs
        t = zip z m
        b = filter (\p -> ((fst p) /= []) && not (checkpairs (snd p) us)) t
resolution_poss2 (Node (x, us) [z]) = resolution_poss2 z


-- | Check if a given pair is an element of a list.
checkpairs :: (DualSeq, DualSeq) -> [(DualSeq, DualSeq)] -> Bool
checkpairs (a,b) [] = False
checkpairs (a,b) (y:ys) = if ((a,b) == y || (b,a) == y) then True else checkpairs (a,b) ys


-- | Given two sequents, function returns list of complementary formulas.
compllist :: DualSeq -> DualSeq -> [For]
compllist (Dual (x, y, z)) (Dual (x1, y1, z1)) = returncompl x x1 ++ returncompl y z1 ++ returncompl z y1

-- | Resolution performed on two given sequents using complementary formulas from a given list.
fun :: [For] -> DualSeq -> DualSeq -> DualSeq
fun (x1:x2:xs) (Dual (z1, z2, z3)) (Dual (y1, y2, y3)) = Dual (rmdups (filter (/= x1) z1 ++ filter (/= x2) y1), z2 ++ y2, z3 ++ y3)


-- | Check if a given pair of sequents is in a memory of a hypersequent.
checkSets :: (DualSeq, DualSeq) -> [(DualSeq, DualSeq)] -> Bool
checkSets (a, b) xs = if (a3 `elem` a4) then True else False
    where
      a1 = sets a
      a2 = sets b
      a3 = (a1,a2)
      a4 = map (\(x,y) -> (sets x, sets y)) xs

-- | Resolution: find a pair of sequents on which a resolution can be performed.
{-rez :: [(DualSeq, DualSeq)] -> ([DualSeq], [(DualSeq, DualSeq)])
rez []           = []
rez (x:xs)
    | c == []    = ([], unpair [x] ++ rez xs)
    | otherwise  = if (d `elem` (unpair (x:xs))) then unpair [x] ++ rez xs else d:unpair (x:xs)
      where
        a = fst x
        b = snd x
        c = compllist a b
        d = fun c a b
-}
-- | Perform a resolution iff a given pair does not exists in memory.
rez1 :: ([(DualSeq, DualSeq)], [(DualSeq, DualSeq)]) -> ([DualSeq], [(DualSeq, DualSeq)])
rez1 ([], y)        = ([], y)
rez1 ((x:xs), us)   = if (checkSets x us || checkSets (swap x) us)
                        then addPair x (rez1 (xs, us))
                        else if c == [] then addPair x (rez1 (xs, us)) else if (d `elem` (unpair (x:xs))) then addPair x (rez1 (xs, us)) else (d:unpair (x:xs), x:us)
                        where
                            a = fst x
                            b = snd x
                            c = compllist a b
                            d = fun c a b


addPair :: (DualSeq, DualSeq) -> ([DualSeq], [(DualSeq, DualSeq)]) -> ([DualSeq], [(DualSeq, DualSeq)])
addPair (a,b) (xs,ys) = (a:b:xs, ys)

-- | Check if two sequents and its contents are somewhere on the list.
findPair :: [(DualSeq,DualSeq)] -> DualSeq -> DualSeq -> Bool
findPair [] x y   = False
findPair (z:zs) x y = if (pairset z == pairset (x, y) || pairset z == pairset (y, x)) then True else findPair zs x y

-- | Creates sets from lists.
sets :: DualSeq -> Set For
sets (Dual (x,y,z)) = (fromList x)

-- | Creates sets from two sequents in a pair.
pairset :: (DualSeq, DualSeq) -> (Set For, Set For)
pairset (x,y) = (sets x, sets y)

-- | Returns hypersequent.
seek :: Tree ([DualSeq], [(DualSeq, DualSeq)]) -> [DualSeq]
seek (Node (xs, ys) []) = xs
seek (Node (x, us) [z]) = seek z

-- | Checks whether next step of the derivation does not differ from the one before it.
test_cond_tree :: Tree ([DualSeq], [(DualSeq, DualSeq)]) -> Bool
test_cond_tree (Node ((x:xs), us) []) = if (set == fromList (x:xs)) then True else False
    where
      set = fromList s
      s = seek next_step
      next_step = prooftree2x (Node ((x:xs), us) [])
test_cond_tree (Node (x, us) [z]) = test_cond_tree z

-- |
rmdupsrez1 :: ([DualSeq], [(DualSeq, DualSeq)])-> ([DualSeq], [(DualSeq, DualSeq)])
rmdupsrez1 (xs, ys) = (x,y)
    where
        x = rmdups (fst $ (rez1 (pairs' xs, ys)))
        y = snd (rez1 (pairs' xs, ys))




-------------DERIVATIONS---------------------


-- | Building a tree using resolution only, different functions inside.
prooftree2x :: Tree ([DualSeq], [(DualSeq, DualSeq)])-> Tree ([DualSeq], [(DualSeq, DualSeq)])
prooftree2x (Node ([x], us) []) = (Node ([x], us) [])
prooftree2x (Node (xs, us) []) =  Node (xs, us) [Node (rmdupsrez1 (xs, us)) []]
prooftree2x (Node (x,us) xs) = Node (x, us) (map prooftree2x xs)

-- | Building a tree  until a condition is met.
proofDualtestx :: For -> Tree ([DualSeq], [(DualSeq, DualSeq)])
proofDualtestx x = until (der_condx) prooftree2x (cleanTree (derivationF x))

dualProof :: For -> Tree ([DualSeq], [(DualSeq, DualSeq)])
dualProof x = if resolution_poss2 (derivationF x)
              then proofDualtestx x
              else derivationF x


-- | Conditions under which a derivation can be built.
der_condx :: Tree ([DualSeq], [(DualSeq, DualSeq)]) -> Bool
der_condx x
    | closed_tree x = True
    | not (resolution_poss2 x) = True
    | test_cond_tree x = True
    | otherwise = False

-- | Check if a given formula is a tautology.
isTaut :: For -> Bool
isTaut x = closed_tree (proofDualtestx x)


-- | Length of the derivation.
lenOfTree :: Tree ([DualSeq],[(DualSeq, DualSeq)]) -> Int
lenOfTree (Node x []) = 1
lenOfTree (Node x rest) = 1 + lenOfTree (head rest)

-- | Length of the derivation.
lenOfDualTree :: Tree ([DualSeq], [(DualSeq, DualSeq)]) -> Int
lenOfDualTree (Node (x, y) []) = if y == [] then 1 else 0
lenOfDualTree (Node (x, y) rest) = if y /= [] then 0 + lenOfDualTree (head rest) else
                                      1 + lenOfDualTree (head rest)

-- | Number of times resolution rule has been applied.
resNum :: Tree ([DualSeq], [(DualSeq, DualSeq)]) -> Int
resNum x = lenOfTree x - lenOfDualTree x

--example test

-- ~~~~(p v ~p)
resEx = N (N (N (N (V 1 `D` N (V 1)))))
resEx1 = proofDualtestx resEx


-- Module for cleaning clausal form

-- creates a list of formulas of the given sequent
seqtolist :: DualSeq -> [For]
seqtolist (Dual (x, y, z)) = x ++ y ++ z

-- | Checks if a any formula from the given list is an element of a sequent.
elemseq' :: [For] -> DualSeq -> Bool
elemseq' xs y = any (\x -> elem x xs) (seqtolist y)


-- NARROWING THE SPACE
-- checks if there are complementary formulas in a sequent
checkcompl :: DualSeq -> Bool
checkcompl z = if b /= [] then True else False
    where
        w = (seqtolist z)
        b = returncompl w w

-- subsumption, checks if all formulas of one sequent are included in another sequent
subsump :: (DualSeq,DualSeq) -> Bool
subsump (x, y) = all (\a -> elem a ys) xs
    where
        xs = seqtolist x
        ys = seqtolist y


-- |Takes a list of sequents and returns a list of 'lone formulas'.
{-
lonefor :: [DualSeq] -> [For]
lonefor [] = []
lonefor w =
	where
		x = map seqtolist w
		y = seqtolist (tail x)
		fun1 :: [For] -> [[For]] -> Bool
		fun1 z:zs v =

-}
-- |Takes a list of sequents and returns a list of 'lone formulas'.
lonefor :: [DualSeq] -> [For]
lonefor [] = []
lonefor w = checks y x
  where
    x = map seqtolist w
    y = length x
    checks :: Int -> [[For]] -> [For]
    checks n [x] = x
    checks 0 (xs) = []
    checks n (x:xs) = [a | a <- x, var a, (not (elem a (concat xs))) && (not ((compl1 a) `elem` (concat xs)))] ++ checks (n-1) (xs ++ [x])


przy = lonefor' (lonefor [Dual ([V 1],[],[]), Dual ([N (V 1)],[],[]), Dual ([V 7], [], []),  Dual ([V 9], [], [])]) [Dual ([V 1],[],[]), Dual ([N (V 1)],[],[]), Dual ([V 7], [], []),  Dual ([V 9], [], [])] []

przy1 = cleanseq [Dual ([V 1],[],[]), Dual ([N (V 1)],[],[]), Dual ([V 7], [], []),  Dual ([V 9], [], [])]



{-
apply_n_times :: Int -> ([DualSeq] -> [For]) -> [DualSeq] -> [For]
apply_n_times 0 f x = x
apply_n_times n f x = apply_n_times (n-1) f (f x)
-}
--apply_n_times (length [DualSeq]) -> lonefor -> [DualSeq]

compl1 :: For -> For
compl1 x =  case x of
 N z -> z
-- N z -> N (N z)
 z -> N z


--        checks (x:xs) = [a | a <- x, (not (elem a (concat xs))) || (not ((elem (head (compl a)) (concat xs))) && not ((elem (head (tail (compl a))) (concat xs))))] ++ checks xs

-- Gives a list of sequents NOT containing 'lone formulas'.
lonefor' :: [For] -> [DualSeq] -> [DualSeq] -> [DualSeq]
lonefor' [] b c   = b
lonefor' _ [] c   = c
lonefor' xs (y:ys) c = if elemseq' xs y then lonefor' xs ys c else lonefor' xs ys (c++[y])


-- pairs'
--z - taut, y,w - subsump,
cleanseq :: [DualSeq] -> [DualSeq]
cleanseq xs = w
   where
       z = filter (\x -> not (checkcompl x)) xs
       y = pairs g
       w = filterpairs' (sortseq y)
       filterpairs' :: [(DualSeq,DualSeq)] -> [DualSeq]
       filterpairs' [] = []
       filterpairs' ((a,b):ys)
         | subsump (a,b) = a:filterpairs' ys
         | subsump (b,a) = b:filterpairs' ys
         | otherwise     = a:b:filterpairs' ys
       g = lonefor' (lonefor z) z []

cleanTree :: Tree ([DualSeq], [(DualSeq, DualSeq)]) -> Tree ([DualSeq], [(DualSeq, DualSeq)])
cleanTree (Node (x, []) []) = Node (x, []) [Node ((cleanseq x), []) []]
cleanTree (Node (x, []) (ys)) = Node (x, []) (map cleanTree ys)

---------------------

seqsum :: DualSeq -> DualSeq -> Int
seqsum x y = length (seqtolist x) + length (seqtolist y)

sortseq :: [(DualSeq, DualSeq)] -> [(DualSeq, DualSeq)]
sortseq xs = sortBy (\(x, y) (z, v) -> compare (seqsum x y) (seqsum z v)) xs

-- | Number of sequents in final hypersequent.
numOfSeq :: Tree ([DualSeq], [(DualSeq, DualSeq)]) -> Int
numOfSeq (Node (x,y) []) = length x
numOfSeq (Node x rest) = numOfSeq (head rest)
{-
Node {rootLabel = ([Dual ([],[V 1 `I` V 1],[])],[]),
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([V 1],[],[])],[]),
	subForest = [
		Node {rootLabel = ([Dual ([],[],[]),Dual ([N (V 1)],[],[]),Dual ([V 1],[],[])],[(Dual ([N (V 1)],[],[]),Dual ([V 1],[],[]))]),
		subForest = []}]}]}

-}


{-
*Main> Dual.proofDualtestx  ((V 0 `I` ((V 0) `A` ((V 1) `I` ((V 1) `I` N (V 1))))))

[[p → (p & (q → (q → ~q)))]] [pusta pamiec]
[[~p], [(p & (q → (q → ~q)))]] [pusta pamiec]
[[~p], [p, (q → (q → ~q))]] [pusta pamiec]
[[~p], [p, ~q], [p, (q → ~q)]] [pusta pamiec]
[[~p], [p, ~q], [p, ~q], [p, ~q]] [pusta pamiec]
[[~q], [~p], [~q, p]] [([~p], [~q, p])]


-}

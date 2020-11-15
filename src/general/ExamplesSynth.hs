module ExamplesSynth where

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
--import qualified Text.LaTeX.Packages.Trees.Qtree as Qtree

-- Michał - przykład
mich0 = N (V 1) `I` (V 2)
mich = (N (N (V 1)) `I` (V 1)) `I` N (N (N (V 1) `I` (V 2)))
mich1 = [[V 1], [V 2, V 2]]
mich2 = synthFlex mich1 mich
mich3 = putStrLn $ drawTree (prePrint mich2)

m0 = [[V 1], [V 2, V 2]]
m1 = (N (V 1) `I` N (N (N (V 1)))) `I` (N (V 1) `I` (V 2))
m2 = synthFlex m0 m1
m3 = putStrLn $ drawTree (prePrint m2)

mm0 = [[V 2], [V 1, V 1]]
mm1 = ((N (V 1) `I` (V 1)) `I` N (V 2)) `I` (N (V 2) `I` (V 1))
mm2 = synthFlex mm0 mm1
mm3 = putStrLn $ drawTree (prePrint mm2)
mm4 = map printFor1 (sortComplexity (subFormulas mm1))

-- Examples -- flexible

a0 = [[V 1], [V 2, V 3], [V 3, V 3, V 2, V 2]]
a1 = (V 3) `A` (V 3)
a2 = synthFlex a0 a1
a3 = putStrLn $ drawTree (prePrint a2)

-- (V 1) `I` ((V 2) `I` (V 3))
-- (V 0) `I` ((V 1) `I` ((V 2) `I` (V 3)))
--

debug = applyRuleFlex ((V 1) `I` (V 2)) (sortComplexity (subFormulas ((V 1) `I` (V 2)))) (cutRuleFlex [V 1] (Node [Nothing] []))
debug1 = putStrLn $ drawTree (prePrint debug)


threeProp = [[V 1, N (V 1)], [V 2, N (V 2),V 3, N (V 3)],[V 3, N (V 3), V 3, N (V 3), V 2, N (V 2),V 2, N (V 2)]]

expandLeaf :: [a] -> Tree a -> Tree a
expandLeaf [x] (Node z [])          = Node z [Node x []]
expandLeaf [x,y] (Node z [s1, s2])  = Node z [expandLeaf [x] s1, expandLeaf [y] s2]


expandLeaf1 :: [For] -> Tree For -> Tree For
expandLeaf1 [x] (Node z [])          = Node z [Node x [], Node (N x) []]
expandLeaf1 [x,y] (Node z [s1, s2])  = Node z [expandLeaf1 [x] s1, expandLeaf1 [y] s2]



ameliaFor1 = ( ( ( (V 1) `D` (V 2) ) `I` (V 3) ) `D` (V 4) ) `I` ( ( (V 4) `A` (V 3) ) `I` ( (V 1) `I` (V 2) ) ) -- (p v q -> r) v s -> (s & r -> (p -> q))
ameliaTree = synth11 ameliaFor1
ameliaFor11 = putStrLn $ drawTree (prePrint1 ameliaTree)
ameliataut = tautology ameliaFor1
num = numberOfBranches ameliaTree

p = V 1
q = V 2
r = V 3
s = V 4
t = V 5

ameliaFor2 = ( ( (p) `D` (q) ) `A` ( ( (p) `A` (r) ) `I` (q) ) ) `I` ( ( (r) `I` (s) ) `I` ( ( (q) `D` (p) ) `A` ( ( (r) `D` (s) ) `I` (t) ) ) )

wow = allTT ameliaFor2

wow1 = allTT ameliaFor1

--EXAMPLES

-- synth0 = synth ((N ((V 1) `E` (V 2))) `E` (N ((V 1) `E` (V 2))))
-- example0 = putStrLn $ drawTree (prePrint synth0)
-- example00 = prePrint synth0
-- tautology0 = tautology ((N ((V 1) `E` (V 2))) `E` (N ((V 1) `E` (V 2))))
--
-- synth00 = synth11 ((N ((V 1) `E` (V 2))) `E` (N ((V 1) `E` (V 2))))
-- example000 = putStrLn $ drawTree (prePrint synth00)
-- example0000 = prePrint synth00
-- tautology00 = tautology ((N ((V 1) `E` (V 2))) `E` (N ((V 1) `E` (V 2))))
--
-- synth1 = synth (((V 1) `E` (V 2)) `E` ((V 1) `E` (V 2)))
-- example1 = putStrLn $ drawTree (prePrint synth1)
-- example11 = prePrint synth1
-- tautology1 = tautology (((V 1) `E` (V 2)) `E` ((V 1) `E` (V 2)))
--
-- synth2 = synth (N (V 1) `E` N (V 1))
-- example2 = putStrLn $ drawTree (prePrint synth2)
-- tautology2 = tautology (N (V 1) `E` N (V 1))
--
-- synth3 = synth (N ((V 1) `E` (N (V 1))))
-- example3 = putStrLn $ drawTree (prePrint synth3)
-- tautology3 = tautology (N ((V 1) `E` (N (V 1))))
--
-- synth4 = synth (((V 1) `E` (V 2)) `E` ((V 2) `E` (V 1)))
-- example4 = putStrLn $ drawTree (prePrint synth4)
-- tautology4 = tautology (((V 1) `E` (V 2)) `E` ((V 2) `E` (V 1)))
--
-- synth5 = synth11 (((V 1) `E` (V 2)) `E` (N (V 2) `E` N (V 1)))
-- example5 = putStrLn $ drawTree (prePrint1 synth5)
-- tautology5 = tautology (((V 1) `E` (V 2)) `E` (N (V 2) `E` N (V 1)))
--
-- synth6 = synth11 (((V 1) `E` (V 1)) `E` ((V 2) `E` (V 2)))
-- example6 = putStrLn $ drawTree (prePrint1 synth6)
-- tautology6 = tautology (N ((V 1) `E` (N (V 1))))
-- example66 = prePrint1 synth6
--
-- synth7 = synth11 ((((V 1) `E` (V 1)) `E` ((V 2) `E` (V 2))) `E` ((V 3) `E` (V 3)))
-- example7 = putStrLn $ drawTree (prePrint1 synth7)
-- tautology7 = tautology (N ((V 1) `E` (N (V 1))))
--
-- synth8 = synth11 (I (V 1) (V 1))
-- example8 = putStrLn $ drawTree (prePrint1 synth8)
-- tautology8 = tautology (I (V 1) (V 1))
--
-- synth9 = synth11 (N (A (V 1) (N (V 1))))
-- example9 = putStrLn $ drawTree (prePrint1 synth9)
-- tautology9 = tautology (N (A (V 1) (N (V 1))))
--
-- synth10 = synth11 (D (V 1) (N (V 1)))
-- example10 = putStrLn $ drawTree (prePrint1 synth10)
-- tautology10 = tautology (D (V 1) (N (V 1)))
--
-- synth12 = synth11 ((N ((V 1) `A` (V 2))) `I` ( (N (V 1) ) `D` (N (V 2) ) ) )
-- example12 = putStrLn $ drawTree (prePrint1 synth12)
-- tautology12 = tautology ((N ((V 1) `A` (V 2))) `I` ( (N (V 1) ) `D` (N (V 2) ) ) )
--
-- synth13 = synth11 ((N ((V 1) `D` (V 2))) `I` ( (N (V 1) ) `A` (N (V 2) ) ) )
-- example13 = putStrLn $ drawTree (prePrint1 synth13)
-- tautology13 = tautology ((N ((V 1) `D` (V 2))) `I` ( (N (V 1) ) `A` (N (V 2) ) ) )
--
-- synth14 = synth11 (( N ( (V 1) `I `(V 2) ) ) `I` (A (V 1) (N (V 2) )))
-- example14 = putStrLn $ drawTree (prePrint1 synth14)
-- tautology14 = tautology (( N ( (V 1) `I `(V 2) ) ) `I` (A (V 1) (N (V 2) )))
--
-- synthAmelia1 = synth11 (((V 1) `I` (V 2)) `I` (((V 1) `I` (V 3)) `I` ((V 1) `I` ((V 2) `A` (V 3)))))
-- exampleAmelia1 = putStrLn $ drawTree (prePrint1 synthAmelia1)
-- tautologyAmelia1 = tautology (((V 1) `I` (V 2)) `I` (((V 1) `I` (V 3)) `I` ((V 1) `I` ((V 2) `A` (V 3)))))
--
-- synthAmelia2 = synth11 (((V 1) `I` (V 2)) `I` (((V 1) `I` (V 3)) `I` ((V 2) `A` (V 3))))
-- exampleAmelia2 = putStrLn $ drawTree (prePrint1 synthAmelia2)
-- tautologyAmelia2 = tautology (((V 1) `I` (V 2)) `I` (((V 1) `I` (V 3)) `I` ((V 2) `A` (V 3))))

res = map (drawTree.prePrint1) (allT ameliaFor1)
res1 = map (drawTree.prePrint1) (allT ameliaFor2)

results = writeFile "amelia1.txt" $ unlines res
results1 = writeFile "amelia2.txt" $ unlines res1

diff = writeFile "diff.txt" $ printFor (Just ameliaFor1) ++ "\n"
difff = appendFile "diff.txt" $ unlines res

marc = allT ameliaFor1

marcc = map (show) marc

marc1 = writeFile "dane_marcin1.txt" $ unlines marcc

amelia_all_trees = allTFlex (I (N (V 1)) (D (V 2) (N (V 3) ) ) )

a = Node {rootLabel = [Nothing], subForest = [Node {rootLabel = [Just (V 1),Nothing], subForest = [Node {rootLabel = [Just (V 1),Nothing,Just (N (N (V 1)))], subForest = [Node {rootLabel = [Just (V 1),Nothing,Just (N (N (V 1))),Just (N (V 1 `E` N (V 1)))], subForest = []}]}]},Node {rootLabel = [Just (N (V 1)),Nothing], subForest = [Node {rootLabel = [Just (N (V 1)),Nothing,Just (N (V 1 `E` N (V 1)))], subForest = []}]}]}

--Examples, Problematic cases
--((p5) -> (p1)) & (p4)
-- {5}{14}{4411}
--instruction = [[V 5], [V 1, V 4], [V 4, V 4, V 1, V 1]]
--formula = ((V 5) `I` (V 1)) `A` (V 4)
--der = synthFlex instruction formula

ins = [[V 3],[V 2, V 1],[V 1, V 1, V 2, V 2]]
for = (V 1) `A` ((V 2) `I` (V 3))
der = synthFlex ins for
derr = putStrLn $ drawTree (prePrint1 der)

ins1 = [[V 1], [V 2, V 2]]
for1 = N ((V 1) `I` (V 2))
der1 = synthFlex ins1 for1
derr1 = putStrLn $ drawTree (prePrint1 der1)
--resultsMod =  "amelia1.txt" (putStrLn ())

--results1 = readFile $ show "amelia.txt"
--
--
-- --
-- instruction :: [For] -> [[[For]]]
-- instruction [x,y,z] = [[[x],[y,z],[z,z,y,y]], [[x],[y,y],[z,z,z,z]]]
-- -- --instruction (n:ns)  =
-- -- --[[x],[z,y],[y,y,z,z]]
-- --
-- instructions :: [For] -> [[[For]]]
-- instructions xs = concat $ map instruction (permuteAtoms xs)

--
-- rule1 :: [For] -> [[[For]]]
-- rule1 [x,y,z] = [[[x],[y,y],[z,z,z,z]]]
-- rule1 (x:xs)   = [[x] : (mergeFlex (a) (b)) | a <- base, b <- base]
--      where
--        base = rules1 xs
--
--rules :: [For] -> [[[For]]]

--
-- rules1 :: [For] -> [[[For]]]
-- rules1 xs = concat $ map rule1 (permuteAtoms xs)

-- inst :: [For] -> [[[For]]]
-- inst [x,y,z,w] =  [[x] : (mergeFlex (a) (b)) | a <- instructions base, b <- instructions base]
--     where
--       base = [y,z,w]
--
-- -- [V 1, V 2, V 3, V 4]
-- -- [[V 1], [V 2, V 2], [V 3, V 3, V 3, V 3], [V 4, V 4, V 4, V 4, V 4, V 4, V 4, V 4]]
--

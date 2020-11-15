module PrintingTrees
--  (
--    printFor
--  , prePrint
--  , prePrint1
--  )
where

import Language
import CanonicalTrees
import OptimizedTrees
--import FlexibleTrees
import Data.Tree
import Data.List
import Text.PrettyPrint
--import qualified Text.LaTeX.Packages.Trees.Qtree as Qtree

printFor1 :: For -> String
printFor1 for = printFor (Just for)

printFor11 :: [For] -> [String]
printFor11 xs = map printFor1 xs

printFor111 :: [[For]] -> [[String]]
printFor111 xs = map printFor11 xs

printFor :: MF -> String
printFor for = case for of
  Nothing    -> ""
  Just x     -> case x of
    (V y)    -> "p" ++ (show y)
    (N y)    -> "~" ++ "(" ++ (printFor (Just y)) ++ ")"
    (E y z)  -> "(" ++ printFor (Just y) ++ ")" ++ " = " ++ "(" ++ printFor (Just z) ++ ")"
    (I y z)  -> "(" ++ printFor (Just y) ++ ")" ++ " -> " ++ "(" ++ printFor (Just z) ++ ")"
    (A y z)  -> "(" ++ printFor (Just y) ++ ")" ++ " & " ++ "(" ++ printFor (Just z) ++ ")"
    (D y z)  -> "(" ++ printFor (Just y) ++ ")" ++ " v " ++ "(" ++ printFor (Just z) ++ ")"

prePrint :: MT -> Tree String
prePrint tree = fmap (prntOpt) tree
  where
    prntOpt :: [MF] -> String
    prntOpt [x]     = printFor x
    prntOpt (x:xs)  = case x of
      Nothing   -> case xs of
        [Nothing]   -> " "
        [Just z]    -> printFor (Just z)
        _           -> prntOpt xs
      Just y    -> case xs of
        [Nothing]   -> printFor x
        [Just z]    -> printFor x ++ ", " ++ printFor (Just z)
        _           -> printFor x ++ ", " ++ prntOpt xs

prePrint1 :: MT-> Tree String
prePrint1 tree = fmap (prntOpt) tree
    where
      prntOpt :: [MF] -> String
      prntOpt [x]     = printFor x
      prntOpt (x:xs)  = case x of
        Nothing   -> case xs of
          [Nothing]   -> " "
          [Just z]    -> printFor (Just z)
          _           -> printFor (last xs)
        Just y    -> case xs of
          [Nothing]   -> printFor x
          [Just z]    -> printFor (Just z)
          _           -> printFor x ++ ", " ++ printFor (last xs)

module PrintingMbC where

import LanguageMbC
import Data.Tree
import Generator
--import Sort
import Text.PrettyPrint

lookupOne :: Int -> [(Int, String)] -> String
lookupOne x list = head [values | (key,values)<-list, x==key]

variables :: [(Int, String)]
variables = zip [0..12] ["p", "q", "r", "s", "t", "u", "v", "x", "y", "z"]

-- printing formulas
printFor :: For -> String
printFor for = case for of
    (Verum)  -> "Verum"
    (V i)    -> lookupOne i variables  -- chwilowo wygodniejsze wyświetlanie
    (N y)    -> case y of
      (V z)   -> "~" ++ printFor (V z)
      (N z)   -> "~" ++ printFor (N z)
      _       -> "~" ++ "(" ++ (printFor (y)) ++ ")"
    (E y z)  -> printVar y ++ " = " ++ printVar z
    (I y z)  -> printVar y ++ " -> " ++ printVar z
    (A y z)  -> printVar y ++ " & " ++ printVar z
    (D y z)  -> printVar y ++ " v " ++ printVar z

printVar :: For -> String
printVar x = case x of
  (V i)   -> printFor x
  N p     -> printFor x
  _       -> "(" ++ printFor x ++ ")"

mapPrint :: [For] -> String
mapPrint (x:xs) = "[" ++ printFor x ++ ", "  ++ mapPrint xs
mapPrint [] = "]"

printSeq :: CanSeq -> String
printSeq  (Can (x, y, z)) = "Can " ++ "([" ++ mapPrint x  ++ "]" ++ ",[" ++ mapPrint y  ++ "]" ++ ",[" ++ mapPrint z  ++ "])"

mapPrint2 :: [CanSeq] -> String
mapPrint2 (x:xs) = "[" ++ printSeq x ++ ", "  ++ mapPrint2 xs
mapPrint2 [] = "]"


canseq2string :: Tree [CanSeq] -> Tree String
canseq2string (Node x []) = Node (mapPrint2 x) []
canseq2string (Node x xs) = Node (mapPrint2 x) (map canseq2string xs)

{-
(p & (((((p -> q)=(r=s))=(t -> r))v ~s)vs))

(p -> (~((((q & r) & r) & (r v q)) & s) -> (r v t)))
-}

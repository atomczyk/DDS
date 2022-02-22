module Parsing where

import Language
import Generator
--import Data.Tree
--import Printing
--import Text.PrettyPrint

parseM :: String -> [Char]
parseM []    = []
parseM (x:xs) = case x of
  '"'     -> parseM xs
  '('     -> '(' : parseM xs
  ')'     -> ')' : parseM xs
  'p'     -> "(V 0)" ++ parseM xs
  'q'     -> "(V 1)" ++ parseM xs
  'r'     -> "(V 2)" ++ parseM xs
  's'     -> "(V 3)" ++ parseM xs
  't'     -> "(V 4)" ++ parseM xs
  'u'     -> "(V 5)" ++ parseM xs
  '~'     -> "N " ++ parseM xs
  '&'     -> " `A` " ++ parseM xs
  'v'     -> " `D` " ++ parseM xs
  '-'     -> " `I` " ++ parseM (tail xs)

parseMM :: String -> For
parseMM s = read (parseM s) :: For

for1 = "(((((((p->q)&r)&s)&p)v(tv~p))&p)&t)vq"
for2 = "(((((((p->q)&r)&s)&p)v(tvp))&p)&t)"
for3 = "(((((p->q)&r)&s)&p)v(tvp))"

-- SC dodane "head xs == 'b'" jako cłon alternatywy, żeby ogarnąć nowy plik od michała z kolumną "brak"
parseIns :: String -> [Char]
parseIns []    = [']']
parseIns (x:xs) = case x of
  '"'     -> if (tail xs == [] || head xs == ',' ) then ']': parseIns xs else '[':parseIns xs
--  'b'     -> "(V 0)" ++ parseIns xs
--  '"'     -> if (head xs == ',' || tail xs == []) then ']': parseIns xs else if (head xs == 'b') then [']'] else '[':parseIns xs
  'p'     -> "(V 0)" ++ parseIns xs
  'q'     -> "(V 1)" ++ parseIns xs
  'r'     -> "(V 2)" ++ parseIns xs
  's'     -> "(V 3)" ++ parseIns xs
  't'     -> "(V 4)" ++ parseIns xs
  'u'     -> "(V 5)" ++ parseIns xs
  ','     -> ',': parseIns xs
  '\r'    -> parseIns xs

--(head xs == 'b' || head xs == 'r' || head xs == 'a' || head xs == 'k') then ("(V 0)" ++ parseIns xs) else

parseIns1 x = '[':parseIns x

instr = "\"p,q,s,t\",\"p,q,s,t\",\"p,q,s,t\",\"p,q,s,t\",\"b\"\r"

-- parsowanie instrukcji
parseI x = read (parseIns1 x) :: [[For]]

for = ((((((((V 1) `I` (V 2)) `A` (V 3)) `A` (V 4)) `A` (V 1)) `D` ((V 5) `D` N (V 1))) `A` (V 1)) `A` (V 5)) `D` (V 2)

module Generator where

import Control.Monad
--import Text.Parsec
--import Text.Parsec.String
import Control.Applicative
import Data.Char
import Data.Set hiding (filter, map, drop)
import Data.Maybe
import Data.List
import Language


data AbstractCPL = Op1 | Op2 | Var | L | R deriving(Eq, Read, Show,  Ord)

 
-- | Sets of variable combinations
var0 = [["(V 1)"],["(V 1)"]]
var1 = [["(V 1)"]]
var2 = [["(V 1)", "(V 1)"], ["(V 1)", "(V 2)"]]
var3 = [["(V 1)", "(V 1)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)"], ["(V 2)", "(V 1)", "(V 1)"], ["(V 1)", "(V 2)", "(V 1)"],["(V 1)", "(V 2)", "(V 3)"]]
var4 = [["(V 1)", "(V 1)", "(V 1)", "(V 1)"], ["(V 1)", "(V 1)", "(V 1)", "(V 2)"], ["(V 2)", "(V 1)", "(V 1)", "(V 1)"], ["(V 1)", "(V 2)", "(V 1)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)"], ["(V 2)", "(V 1)", "(V 1)", "(V 3)"], ["(V 2)", "(V 3)", "(V 1)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)"], ["(V 2)", "(V 1)", "(V 3)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)", "(V 2)"], ["(V 2)", "(V 1)", "(V 1)", "(V 2)"], ["(V 2)", "(V 1)", "(V 2)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)"]]
var5 = [["(V 1)", "(V 1)", "(V 1)", "(V 1)", "(V 1)"], ["(V 1)", "(V 1)", "(V 1)", "(V 1)", "(V 2)"], ["(V 2)", "(V 1)", "(V 1)", "(V 1)", "(V 1)"], ["(V 1)", "(V 2)", "(V 1)", "(V 1)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)", "(V 1)", "(V 1)"], ["(V 1)", "(V 1)", "(V 1)", "(V 2)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)", "(V 2)", "(V 2)"], ["(V 1)", "(V 1)", "(V 1)", "(V 2)", "(V 2)"], ["(V 2)", "(V 1)", "(V 1)", "(V 1)", "(V 2)"], ["(V 1)", "(V 2)", "(V 2)", "(V 1)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)", "(V 2)", "(V 1)"], ["(V 1)", "(V 2)", "(V 1)", "(V 2)", "(V 1)"], ["(V 1)", "(V 2)", "(V 1)", "(V 1)", "(V 2)"], ["(V 1)", "(V 1)", "(V 2)", "(V 1)", "(V 2)"], ["(V 2)", "(V 1)", "(V 1)", "(V 2)", "(V 1)"], ["(V 2)", "(V 1)", "(V 2)", "(V 1)", "(V 1)"], ["(V 1)", "(V 1)", "(V 1)", "(V 2)", "(V 3)"], ["(V 2)", "(V 1)", "(V 1)", "(V 1)", "(V 3)"], ["(V 2)", "(V 3)", "(V 1)", "(V 1)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 1)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 1)"], ["(V 1)", "(V 2)", "(V 1)", "(V 1)", "(V 3)"], ["(V 1)", "(V 1)", "(V 2)", "(V 1)", "(V 3)"], ["(V 2)", "(V 1)", "(V 1)", "(V 3)", "(V 1)"], ["(V 2)", "(V 1)", "(V 3)", "(V 1)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)", "(V 2)", "(V 3)"], ["(V 2)", "(V 1)", "(V 1)", "(V 3)", "(V 3)"], ["(V 2)", "(V 3)", "(V 1)", "(V 1)", "(V 2)"], ["(V 2)", "(V 2)", "(V 1)", "(V 3)", "(V 1)"], ["(V 2)", "(V 3)", "(V 2)", "(V 1)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 1)"], ["(V 1)", "(V 2)", "(V 2)", "(V 1)", "(V 3)"], ["(V 2)", "(V 1)", "(V 3)", "(V 3)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 3)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 2)"], ["(V 1)", "(V 2)", "(V 1)", "(V 2)", "(V 3)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 2)"], ["(V 1)", "(V 3)", "(V 2)", "(V 1)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 3)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 4)"], ["(V 2)", "(V 1)", "(V 1)", "(V 3)", "(V 4)"], ["(V 2)", "(V 3)", "(V 1)", "(V 1)", "(V 4)"], ["(V 2)", "(V 3)", "(V 4)", "(V 1)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 1)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 4)"], ["(V 2)", "(V 1)", "(V 3)", "(V 4)", "(V 1)"], ["(V 2)", "(V 1)", "(V 3)", "(V 1)", "(V 4)"], ["(V 2)", "(V 3)", "(V 1)", "(V 4)", "(V 1)"]]
var6 = [["(V 1)","(V 1)","(V 1)","(V 1)","(V 1)","(V 1)"],["(V 1)","(V 1)","(V 1)","(V 1)","(V 1)","(V 2)"],["(V 2)","(V 1)","(V 1)","(V 1)","(V 1)","(V 1)"],["(V 1)","(V 2)","(V 1)","(V 1)","(V 1)","(V 1)"],["(V 1)","(V 1)","(V 2)","(V 1)","(V 1)","(V 1)"],["(V 1)","(V 1)","(V 1)","(V 2)","(V 1)","(V 1)"],["(V 1)","(V 1)","(V 1)","(V 1)","(V 2)","(V 1)"],["(V 1)","(V 1)","(V 1)","(V 1)","(V 2)","(V 2)"],["(V 2)","(V 1)","(V 1)","(V 1)","(V 1)","(V 2)"],["(V 2)","(V 2)","(V 1)","(V 1)","(V 1)","(V 1)"],["(V 1)","(V 2)","(V 2)","(V 1)","(V 1)","(V 1)"],["(V 1)","(V 1)","(V 2)","(V 2)","(V 1)","(V 1)"],["(V 1)","(V 1)","(V 1)","(V 2)","(V 2)","(V 1)"],["(V 1)","(V 1)","(V 2)","(V 1)","(V 1)","(V 2)"],["(V 2)","(V 1)","(V 1)","(V 2)","(V 1)","(V 1)"],["(V 1)","(V 1)","(V 1)","(V 2)","(V 1)","(V 2)"],["(V 2)","(V 1)","(V 1)","(V 1)","(V 2)","(V 1)"],["(V 2)","(V 1)","(V 2)","(V 1)","(V 1)","(V 1)"],["(V 1)","(V 1)","(V 2)","(V 1)","(V 2)","(V 1)"],["(V 1)","(V 2)","(V 1)","(V 2)","(V 1)","(V 1)"],["(V 1)","(V 2)","(V 1)","(V 1)","(V 1)","(V 2)"],["(V 1)","(V 2)","(V 1)","(V 1)","(V 2)","(V 1)"],["(V 1)","(V 1)","(V 1)","(V 2)","(V 2)","(V 2)"],["(V 2)","(V 1)","(V 1)","(V 1)","(V 2)","(V 2)"],["(V 2)","(V 2)","(V 1)","(V 1)","(V 1)","(V 2)"],["(V 1)","(V 2)","(V 1)","(V 1)","(V 2)","(V 2)"],["(V 1)","(V 1)","(V 2)","(V 1)","(V 2)","(V 2)"],["(V 1)","(V 2)","(V 1)","(V 2)","(V 1)","(V 2)"],["(V 1)","(V 2)","(V 1)","(V 2)","(V 2)","(V 1)"],["(V 1)","(V 2)","(V 2)","(V 1)","(V 2)","(V 1)"],["(V 1)","(V 2)","(V 2)","(V 1)","(V 1)","(V 2)"],["(V 1)","(V 1)","(V 2)","(V 2)","(V 1)","(V 2)"], ["(V 1)", "(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 1)"],  ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 1)"],  ["(V 1)", "(V 2)", "(V 1)", "(V 1)", "(V 3)", "(V 1)"],  ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 1)", "(V 1)"],  ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 1)", "(V 1)"], ["(V 1)", "(V 2)", "(V 2)", "(V 2)", "(V 3)", "(V 1)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 2)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 2)", "(V 1)"],  ["(V 1)", "(V 3)", "(V 2)", "(V 2)", "(V 2)", "(V 1)"],  ["(V 1)", "(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 1)"], ["(V 1)", "(V 1)", "(V 3)", "(V 2)", "(V 2)", "(V 1)"], ["(V 1)", "(V 2)", "(V 2)", "(V 1)", "(V 3)", "(V 1)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 1)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 2)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 1)", "(V 1)"], ["(V 1)", "(V 2)", "(V 1)", "(V 2)", "(V 3)", "(V 1)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 2)", "(V 1)"], ["(V 1)", "(V 3)", "(V 1)", "(V 2)", "(V 2)", "(V 1)"], ["(V 1)", "(V 3)", "(V 2)", "(V 1)", "(V 2)", "(V 1)"], ["(V 1)", "(V 3)", "(V 2)", "(V 2)", "(V 1)", "(V 1)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 3)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 3)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 3)", "(V 2)", "(V 1)"], ["(V 1)", "(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 2)"], ["(V 1)", "(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 2)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 2)"], ["(V 1)", "(V 2)", "(V 1)", "(V 1)", "(V 3)", "(V 2)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 1)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 1)", "(V 2)"], ["(V 1)", "(V 2)", "(V 2)", "(V 2)", "(V 3)", "(V 2)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 2)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 2)", "(V 2)"], ["(V 1)", "(V 3)", "(V 2)", "(V 2)", "(V 2)", "(V 2)"], ["(V 1)", "(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 2)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 2)"], ["(V 1)", "(V 1)", "(V 3)", "(V 2)", "(V 2)", "(V 2)"], ["(V 1)", "(V 2)", "(V 2)", "(V 1)", "(V 3)", "(V 2)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 1)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 2)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 1)", "(V 2)"], ["(V 1)", "(V 2)", "(V 1)", "(V 2)", "(V 3)", "(V 2)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 2)", "(V 2)"], ["(V 1)", "(V 3)", "(V 1)", "(V 2)", "(V 2)", "(V 2)"], ["(V 1)", "(V 3)", "(V 2)", "(V 1)", "(V 2)", "(V 2)"], ["(V 1)", "(V 3)", "(V 2)", "(V 2)", "(V 1)", "(V 2)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 3)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 3)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 3)", "(V 2)", "(V 2)"], ["(V 1)", "(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 3)"], ["(V 1)", "(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 3)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 3)"], ["(V 1)", "(V 2)", "(V 1)", "(V 1)", "(V 3)", "(V 3)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 1)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 1)", "(V 3)"], ["(V 1)", "(V 2)", "(V 2)", "(V 2)", "(V 3)", "(V 3)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 2)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 2)", "(V 3)"], ["(V 1)", "(V 3)", "(V 2)", "(V 2)", "(V 2)", "(V 3)"], ["(V 1)", "(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 3)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 3)"], ["(V 1)", "(V 1)", "(V 3)", "(V 2)", "(V 2)", "(V 3)"], ["(V 1)", "(V 2)", "(V 2)", "(V 1)", "(V 3)", "(V 3)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 1)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 2)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 1)", "(V 3)"], ["(V 1)", "(V 2)", "(V 1)", "(V 2)", "(V 3)", "(V 3)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 2)", "(V 3)"], ["(V 1)", "(V 3)", "(V 1)", "(V 2)", "(V 2)", "(V 3)"], ["(V 1)", "(V 3)", "(V 2)", "(V 1)", "(V 2)", "(V 3)"], ["(V 1)", "(V 3)", "(V 2)", "(V 2)", "(V 1)", "(V 3)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 3)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 3)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 3)", "(V 2)", "(V 3)"], ["(V 1)", "(V 1)", "(V 1)", "(V 1)", "(V 2)", "(V 3)"], ["(V 1)", "(V 1)", "(V 1)", "(V 2)", "(V 1)", "(V 3)"], ["(V 1)", "(V 1)", "(V 2)", "(V 1)", "(V 1)", "(V 3)"], ["(V 1)", "(V 2)", "(V 1)", "(V 1)", "(V 1)", "(V 3)"], ["(V 1)", "(V 2)", "(V 2)", "(V 2)", "(V 2)", "(V 3)"], ["(V 1)", "(V 1)", "(V 1)", "(V 2)", "(V 2)", "(V 3)"], ["(V 1)", "(V 1)", "(V 2)", "(V 2)", "(V 1)", "(V 3)"], ["(V 1)", "(V 2)", "(V 2)", "(V 1)", "(V 1)", "(V 3)"], ["(V 1)", "(V 1)", "(V 2)", "(V 2)", "(V 2)", "(V 3)"], ["(V 1)", "(V 1)", "(V 2)", "(V 1)", "(V 2)", "(V 3)"], ["(V 1)", "(V 2)", "(V 1)", "(V 1)", "(V 2)", "(V 3)"], ["(V 1)", "(V 2)", "(V 2)", "(V 2)", "(V 1)", "(V 3)"], ["(V 1)", "(V 2)", "(V 1)", "(V 2)", "(V 1)", "(V 3)"], ["(V 1)", "(V 2)", "(V 2)", "(V 1)", "(V 2)", "(V 3)"], ["(V 1)", "(V 2)", "(V 1)", "(V 2)", "(V 2)", "(V 3)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 1)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 4)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 4)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 1)", "(V 1)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 4)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 4)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 2)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 3)", "(V 4)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 3)", "(V 1)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 4)", "(V 1)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 2)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 4)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 4)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 1)", "(V 2)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 4)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 4)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 2)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 3)", "(V 4)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 3)", "(V 2)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 4)", "(V 2)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 3)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 4)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 4)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 1)", "(V 3)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 4)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 4)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 2)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 3)", "(V 4)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 3)", "(V 3)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 4)", "(V 3)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 4)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 4)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 4)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 1)", "(V 4)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 4)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 4)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 2)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 3)", "(V 4)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 3)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 4)", "(V 4)", "(V 4)"], ["(V 1)", "(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 4)"], ["(V 1)", "(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 4)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 4)"], ["(V 1)", "(V 2)", "(V 1)", "(V 1)", "(V 3)", "(V 4)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 1)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 1)", "(V 4)"], ["(V 1)", "(V 2)", "(V 2)", "(V 2)", "(V 3)", "(V 4)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 2)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 2)", "(V 4)"], ["(V 1)", "(V 3)", "(V 2)", "(V 2)", "(V 2)", "(V 4)"], ["(V 1)", "(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 4)"], ["(V 1)", "(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 4)"], ["(V 1)", "(V 1)", "(V 3)", "(V 2)", "(V 2)", "(V 4)"], ["(V 1)", "(V 2)", "(V 2)", "(V 1)", "(V 3)", "(V 4)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 1)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 1)", "(V 2)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 1)", "(V 4)"], ["(V 1)", "(V 2)", "(V 1)", "(V 2)", "(V 3)", "(V 4)"], ["(V 1)", "(V 2)", "(V 1)", "(V 3)", "(V 2)", "(V 4)"], ["(V 1)", "(V 3)", "(V 1)", "(V 2)", "(V 2)", "(V 4)"], ["(V 1)", "(V 3)", "(V 2)", "(V 1)", "(V 2)", "(V 4)"], ["(V 1)", "(V 3)", "(V 2)", "(V 2)", "(V 1)", "(V 4)"], ["(V 1)", "(V 2)", "(V 2)", "(V 3)", "(V 3)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 2)", "(V 3)", "(V 4)"], ["(V 1)", "(V 2)", "(V 3)", "(V 3)", "(V 2)", "(V 4)"]]


-- | Merging every two list together
mergex :: [[a]] -> [[a]] -> [[a]]
mergex [] ys = []
mergex (x:xs) ys = map (x ++) ys ++ mergex xs ys

-- | Schemata of well formes formulas for every length of formula
len1 = [[L, Var, R]]
len2 = [[L, Op1] ++ head (len1) ++ [R]]
len3 = map (\y -> y ++ [R]) (map (\x -> L:Op1:x) len2) ++
       [[L, Op2] ++ head (len1) ++ head (len1) ++ [R]]
len4 = (map (\y -> y ++ [R]) (map (\x -> L:Op1:x) len3) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len1 len2)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len2 len1)))
len5 = (map (\y -> y ++ [R]) (map (\x -> L:Op1:x) len4 ) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len1 len3)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len2 len2)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len3 len1)))
len6 = map (\y -> y ++ [R]) ((map (\x -> L:Op1:x) len5) ++
       (map (\x -> L:Op2:x) (mergex len1 len4)) ++
       (map (\x -> L:Op2:x) (mergex len2 len3)) ++
       (map (\x -> L:Op2:x) (mergex len3 len2)) ++
       (map (\x -> L:Op2:x) (mergex len4 len1)))
len7 =  map (\y -> y ++ [R]) (map (\x -> L:Op1:x) len6) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len1 len5)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len2 len4)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len3 len3)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len4 len2)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len5 len1))
len8 = map (\y -> y ++ [R]) (map (\x -> L:Op1:x) len7) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len1 len6)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len2 len5)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len3 len4)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len4 len3)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len5 len2)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len6 len1))
len9 = map (\y -> y ++ [R]) (map (\x -> L:Op1:x) len8) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len1 len7)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len2 len6)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len3 len5)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len4 len4)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len5 len3)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len6 len2)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len7 len1))
len10 = map (\y -> y ++ [R]) (map (\x -> L:Op1:x) len9) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len1 len8)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len2 len7)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len3 len6)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len4 len5)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len5 len4)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len6 len3)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len7 len2)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len8 len1))
len11 = map (\y -> y ++ [R]) (map (\x -> L:Op1:x) len10) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len1 len9)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len2 len8)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len3 len7)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len4 len6)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len5 len5)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len6 len4)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len7 len3)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len8 len2)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len9 len1))
len12 = map (\y -> y ++ [R]) (map (\x -> L:Op1:x) len11) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len1 len10)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len2 len9)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len3 len8)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len4 len7)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len5 len6)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len6 len5)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len7 len4)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len8 len3)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len9 len2)) ++
       map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len10 len1))
len13 = map (\y -> y ++ [R]) (map (\x -> L:Op2:x) (mergex len6 len6))


-- | Final product of replacing metavariables `Op2' and `Var' with every combinations of connectives and variables for every length of formula + encoded version.
final1 = map (\x -> concat x) n
        where
            m = map (\x-> parsex x) len1
            n = maprep2xx (maprep1xx m)
final1enc = map rmv00 (map replace final1)


final2 = map (\x -> concat x) n
        where
            m = map (\x-> parsex x) len2
            n = maprep2xx (maprep1xx m)
final2enc = map rmv00 (map replace final2)

final3 = map (\x -> concat x) n
        where
            m = map (\x-> parsex x) len3
            n = maprep2xx (maprep1xx m)
final3enc = map rmv00 (map replace final3)

final4 = map (\x -> concat x) n
        where
            m = map (\x-> parsex x ) len4
            n = maprep2xx (maprep1xx m)
final4enc = map rmv00 (map replace final4)

final5 = map (\x -> concat x) n
        where
            m = map (\x-> parsex x) len5
            n = maprep2xx (maprep1xx m)
final5enc = map rmv00 (map replace final5)

final6 = map (\x -> concat x) n
        where
            m = map (\x-> parsex x) len6
            n = maprep2xx (maprep1xx m)
final6enc = map rmv00 (map replace final6)

final7 = map (\x -> concat x) n
        where
            m = map (\x-> parsex x) len7
            n = maprep2xx (maprep1xx m)
final7enc = map rmv00 (map replace final7)

final8 = map (\x -> concat x) n
        where
            m = map (\x-> parsex  x) len8
            n = maprep2xx (maprep1xx m)
final8enc = map rmv00 (map replace final8)

final9 = map (\x -> concat x) n
        where
            m = map (\x-> parsex x) len9
            n = maprep2xx (maprep1xx m)
final9enc = map rmv00 (map replace final9)

final10 = map (\x -> concat x) n
        where
            m = map (\x-> parsex x) len10
            n = maprep2xx (maprep1xx m)
final10enc = map rmv00 (map replace final10)

final11 = map (\x -> concat x) n
        where
            m = map (\x-> parsex x) len11
            n = maprep2xx (maprep1xx m)
final11enc = map rmv00 (map replace final11)

final12 = map (\x -> concat x) n
        where
            m = map (\x-> parsex x) len12
            n = maprep2xx (maprep1xx m)
final12enc = map rmv00 (map replace final12)

final13 = map (\x -> concat x) n
        where
            m = map (\x-> parsex x) len13
            n = maprep2xx (maprep1xx m)


-- | Saving lists of formulas as files (standard and encoded versions)
all1  = writeFile "forms1.txt"  $ unlines (map show (final1))
all2  = writeFile "forms2.txt"  $ unlines (map show (final2))
all3  = writeFile "forms3.txt"  $ unlines (map show (final3))
all4  = writeFile "forms4.txt"  $ unlines (map show (final4))
all5  = writeFile "forms5.txt"  $ unlines (map show (final5))
all6  = writeFile "forms6.txt"  $ unlines (map show (final6))
all7  = writeFile "forms7.txt"  $ unlines (map show (final7))
all8  = writeFile "forms8.txt"  $ unlines (map show (final8))
all9  = writeFile "forms9.txt"  $ unlines (map show (final9))
all10 = writeFile "forms10.txt" $ unlines (map show (final10))
all12 = writeFile "forms12.txt" $ unlines (map show (final12))

all1'  = writeFile "forms1.txt"  $ unlines (map show (final1enc))
all2'  = writeFile "forms2.txt"  $ unlines (map show (final2enc))
all3'  = writeFile "forms3.txt"  $ unlines (map show (final3enc))
all4'  = writeFile "forms4.txt"  $ unlines (map show (final4enc))
all5'  = writeFile "forms5.txt"  $ unlines (map show (final5enc))
all6'  = writeFile "forms6.txt"  $ unlines (map show (final6enc))
all7'  = writeFile "forms7.txt"  $ unlines (map show (final7enc))
all8'  = writeFile "forms8.txt"  $ unlines (map show (final8enc))
all9'  = writeFile "forms9.txt"  $ unlines (map show (final9enc))
all10' = writeFile "forms10.txt" $ unlines (map show (final10enc))
all11' = writeFile "forms10.txt" $ unlines (map show (final11enc))
all12' = writeFile "forms12.txt" $ unlines (map show (final12enc))


-- | Counting variables (as strings) in a given list
countVarsS :: Num a => [[Char]] -> a
countVarsS [] = 0
countVarsS (x:xs) = if x == "Var" then 1 + countVarsS xs else 0 + countVarsS xs

-- | counting variables in a given list
countVars :: Num a => [AbstractCPL] -> a
countVars [] = 0
countVars (x:xs) = if x == Var then 1 + countVars xs else 0 + countVars xs

-- | Counting logical connectives (Op2) in a given list
countOps :: Num a => [AbstractCPL] -> a
countOps [] = 0
countOps (x:xs) = if x == Op2 then 1 + countOps xs else 0 + countOps xs

-- | Checks if x is a string "Var"
isVarS :: String -> Bool
isVarS x = if x == "Var" then True else False

-- | checks if x is a string "Op2"
isOp2S :: String -> Bool
isOp2S x = if x == "Op2" then True else False

-- | Creates every combination of set of variables.  By modification of this particular function we can obtain formulas of specified length withdifferent number of variables. It should be done with a modificagtion of generating sets (var0, var1, ...)
allvarssets :: [String] -> [[String]]
allvarssets xs | (countVarsS xs) == 1 = var1
               | (countVarsS xs) == 2 = var2
               | (countVarsS xs) == 3 = var3
               | (countVarsS xs) == 4 = var4
               | (countVarsS xs) == 5 = var5
               | otherwise            = var6

-- | Creates every combination of set of connectives
allop2sets :: [String] -> [[String]]
allop2sets xs = replicateM (length (filter (\z -> if z == "Op2" then True else
    False) xs)) ["D "]


-- | Changes AbstractCPL to String (Op1 to "N", the rest just to String "Op2")
parsex :: [AbstractCPL] -> [String]
parsex []     = []
parsex (x:xs)
        | x == Op1 = ["N "] ++ parsex xs
        | x == L   = [" ( "] ++ parsex xs
        | x == R   = [" ) "] ++ parsex xs
        | x == Op2 = [show x] ++ parsex xs
        | x == Var = [show x] ++ parsex xs

-- | Replaces every occurrence of x with y in list (z:zs); for example [V, V] [V1, V2] [A, V, B, V] = [A, V1, B, V2]
replacexx :: Eq a => [a] -> [a] -> [a] -> [a]
replacexx [] [] zs = zs
replacexx (x:xs) [] zs = []
replacexx [] (y:ys) zs = []
replacexx (x:xs) (y:ys) (z:zs) = if z == x then y: replacexx xs ys zs else z:replacexx (x:xs) (y:ys) zs

-- | Mapping replacexx function on a given list of Strings with every possible combination of variables in list (y:ys)
maprep11 :: [String] -> [[String]] -> [[String]]
maprep11 xs [] = []
maprep11 xs (y:ys) = [(replacexx (filter (isVarS) xs) y xs)] ++ maprep11 xs ys

-- | Mapping replacexx function on given list of Strings xs with every possible combination of connectives in list (y:ys)
maprep22 :: [String] -> [[String]] -> [[String]]
maprep22 xs [] = []
maprep22 xs (y:ys) = [(replacexx (filter (isOp2S) xs) y xs)] ++ maprep22 xs ys

-- | Doing the same as maprepp11 on every generated formula x in (x:xs)
maprep1xx :: [[String]] -> [[String]]
maprep1xx [] = []
maprep1xx (x:xs) = (maprep11 x ys) ++ (maprep1xx xs)
                where
                    ys = (allvarssets x)

-- | And again, same thing but with connectives
maprep2xx :: [[String]] -> [[String]]
maprep2xx [] = []
maprep2xx (x:xs) = (maprep22 x ys) ++ (maprep2xx xs)
                where
                    ys = allop2sets x


-- | Replacing different elements with specified code
replace :: String -> [(Int, Int)]
replace (x:xs)
	| x == ' ' || x == 'V' = (0,0): replace xs
	| x == '(' || x == ')' = (0,0):replace xs
	| x == '1' = (0,1):replace xs
	| x == '2' = (0,2):replace xs
	| x == '3' = (0,3):replace xs
	| x == '4' = (0,4):replace xs
	| x == 'I' = (2,0):replace xs
	| x == 'N' = (1,0):replace xs
replace [] = []

-- | Removing encoded spaces
rmv00 :: [(Int, Int)] -> [(Int, Int)]
rmv00 [] = []
rmv00 (x:xs) = if x == (0,0) then rmv00 xs else x:rmv00 xs

-- | Complexity of formulas
forlen :: For -> Int
forlen x = case x of
	V _ -> 1
	N z -> 1 + forlen z
	I n m -> 1 + forlen n + forlen m
	A n m -> 1 + forlen n + forlen m
	D n m -> 1 + forlen n + forlen m
	E n m -> 1 + forlen n + forlen m

-- | Is the formula symmetric
symfor :: For -> Bool
symfor x = case x of
	I n m -> if forlen n == forlen m then True else False
	A n m -> if forlen n == forlen m then True else False
	D n m -> if forlen n == forlen m then True else False
	E n m -> if forlen n == forlen m then True else False
	_ -> False

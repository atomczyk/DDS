module Res_Test2 where

import Language
import Data.Tree
import Sort
import Data.List
import Text.PrettyPrint
import Data.Set hiding (filter, map, null)
import AuxiliaryFunctions
import AuxiliaryFormulas
import Dual3

-- (((p -> q) -> p) -> p) v r

{-
[] [(((p -> q) -> p) -> p) v r] [] PAMIĘĆ []
[] [((p -> q) -> p) -> p] [] | [r] [] [] PAMIĘĆ []
[] [] [N ((p -> q) -> p)] | [p] [] [] | [r] [] [] PAMIĘĆ []
[~p] [p -> q] [] | [p] [] [] | [r] [] [] PAMIĘĆ []
[~p,~p] [] [] | [p] [] [] PAMIĘĆ []
[] [] [] | [p] [] [] | [~p,~p] [] [] PAMIĘĆ [~p,~p] [] [] | [p] [] []
-}

--h2
{-
[] [((~p -> q) -> ((p -> q) -> q)) v r] [] PAMIĘĆ []
1. [] [(~p -> q) -> ((p -> q) -> q)] [] | [r] [] [] PAMIĘĆ []
2. [] [] [N (~p -> q)] | [] [(p -> q) -> q] [] | [r] [] [] PAMIĘĆ []
3. [~q,~p] [] [] | [] [(p -> q) -> q] [] | [r] [] [] PAMIĘĆ []
4. [~q,~p] [] [] | [] [] [N (p -> q)] | [q] [] [] | [r] [] [] PAMIĘĆ []
5. [~q,p] [] [] | [q] [] [] | [~q,~p] [] [] | [q] [] [] | [~q,~p] [] [] | [~q,p] [] [] PAMIĘĆ []
6. [p] [] [] | [q] [] [] | [~q,p] [] [] | [~q,~p] [] [] PAMIĘĆ [~q,p] [] [] | [q] [] []
7. [~q] [] [] | [p] [] [] | [q] [] [] | [~q,~p] [] [] | [~q,p] [] [] PAMIĘĆ [p] [] [] | [~q,~p] [] [] | [~q,p] [] [] | [q] [] []
8. [] [] [] | [~q] [] [] | [p] [] [] | [q] [] [] | [~q,p] [] [] | [~q,~p] [] [] PAMIĘĆ [~q] [] [] | [q] [] [] | [p] [] [] | [~q,~p] [] [] | [~q,p] [] [] | [q] [] []
-}

--(p -> (q & r)) & ~(p & s)
{-
[],[],[(p `I` (q `A` r)) `A` N (p `A` s)] PAMIĘĆ []
[],[N (p `A` s),p `I` (q `A` r)],[] PAMIĘĆ []
[N (p)],[p `I` (q `A` r)],[] | [~s],[p `I` (q `A` r)],[] PAMIĘĆ []
[N (p),N (p)],[],[] | [N (p)],[],[q `A` r] | [~s],[p `I` (q `A` r)],[] PAMIĘĆ []
[N (p),N (p)],[],[] | [r,q,N (p)],[],[] | [~s],[p `I` (q `A` r)],[] PAMIĘĆ []
[N (p),N (p)],[],[] | [r,q,N (p)],[],[] | [N (p),~s],[],[] | [~s],[],[q `A` r] PAMIĘĆ []
[N (p),~s],[],[] | [r,q,~s],[],[] | [r,q,N (p)],[],[] | [r,q,~s],[],[] | [r,q,N (p)],[],[] | [N (p),~s],[],[] | [N (p),N (p)],[],[] | [r,q,~s],[],[] | [N (p),N (p)],[],[] | [N (p),N (p)],[],[]  PAMIĘĆ []
-}

--
{-
[],[],[p `A` ((q `D` r) `A` (~s `D` q))] PAMIĘĆ []
[p],[],[(q `D` r) `A` (~s `D` q)] PAMIĘĆ []
[p],[~s `D` q,q `D` r],[] PAMIĘĆ []
[~s,p],[q `D` r],[] | [q,p],[q `D` r],[] PAMIĘĆ []
[q,~s,p],[],[] | [r,~s,p],[],[] | [q,p],[q `D` r],[] PAMIĘĆ []
[q,q,p],[],[] | [r,~s,p],[],[] | [r,q,p],[],[] |
[r,~s,p],[],[] | [q,q,p],[],[] | [q,~s,p],[],[] |
[r,q,p],[],[] | [q,q,p],[],[] | [q,~s,p],[],[] | [r,~s,p],[],[] PAMIĘĆ []
-}

-- p & ((q v r) & (~s v q))
h01 = dualProof (V 1 `A` ((V 2 `D` V 3) `A` (N (V 4) `D` V 2)))

--derivationF
{-
[q,~s,p],[],[] | [r,~s,p],[],[] | [q,q,p],[],[] | [r,q,p],[],[] PAMIĘĆ []
-}

--cleanTree (derivationF)
{-
[q,q,p],[],[] | [r,~s,p],[],[] |
[r,q,p],[],[] | [r,~s,p],[],[] |
[q,q,p],[],[] | [q,~s,p],[],[] |
[r,q,p],[],[] | [q,q,p],[],[] |
[q,~s,p],[],[] | [r,~s,p],[],[]
PAMIĘĆ []
-}

--bez filterpairs w cleantree
{-
[q,~s,p],[],[] | [r,~s,p],[],[] | [q,q,p],[],[] | [r,q,p],[],[] PAMIĘĆ []
-}

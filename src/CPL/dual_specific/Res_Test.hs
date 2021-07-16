module Res_Test where

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
h1 = proofDualtestx ((((V 1 `I` V 2) `I` V 1) `I` V 1) `D` V 3)

{-
Node {rootLabel = ([Dual ([],[(((V 1 `I` V 2) `I` V 1) `I` V 1) `D` V 3],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[((V 1 `I` V 2) `I` V 1) `I` V 1],[]),Dual ([V 3],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N ((V 1 `I` V 2) `I` V 1)]),Dual ([V 1],[],[]),Dual ([V 3],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[V 1 `I` V 2],[]),Dual ([V 1],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[V 1 `I` V 2],[]),Dual ([N (V 1)],[V 1 `I` V 2],[]),Dual ([V 1],[],[])],[(Dual ([N (V 1)],[V 1 `I` V 2],[]),Dual ([V 1],[],[]))]), 
subForest = []}]}]}]}]}
-}

{- pairs'
[],[(((p -> q) -> p) -> p) v r],[] PAMIĘĆ [] 
[],[((p -> q) -> p) -> p],[], [r],[],[] PAMIĘĆ []  
[],[],[N ((p -> q) -> p)], [p],[],[], [r],[],[] PAMIĘĆ [] 
[~p],[p -> q],[], [p],[],[], [r],[],[] PAMIĘĆ [] 
[~p,~p],[],[], [q,~p],[],[], [p],[],[], [r],[],[] PAMIĘĆ [] 
[],[],[], [~p,~p],[],[], [q,~p],[],[], [r],[],[], [p],[],[] 
PAMĘĆ [[~p,~p],[],[], [q,~p],[],[]] 
-}

{- pairs
[],[(((p -> q) -> p) -> p) v r],[] PAMIĘĆ [] 
[],[((p -> q) -> p) -> p],[], [r],[],[] PAMIĘĆ []
[],[],[N ((p -> q) -> p)], [p],[],[], [r],[],[] PAMIĘĆ [] 
[~p],[p -> q],[], [p],[],[], [r],[],[] PAMIĘĆ []
[~p,~p],[],[], [q,~p],[],[], [p],[],[], [r],[],[] PAMIĘĆ [] 
[q],[],[], [r],[],[], [p],[],[], [~p,~p],[],[], [q,~p],[],[]
PAMIĘĆ [[p],[],[], [r],[],[]] 
[],[],[], [q,~p],[],[], [~p,~p],[],[], [p],[],[], [q],[],[], [r],[],[]
PAMIĘĆ [[~p,~p],[],[], [q,~p],[],[] ,[p],[],[],[r],[],[]]
-}

-- (~p -> q) -> ((p -> q) -> q) v r
h2 = proofDualtestx ((N(V 1) `I` V 2) `I` ((V 1 `I` V 2) `I` V 2) `D` V 3)

{-
Node {rootLabel = ([Dual ([],[(N (V 1) `I` V 2) `I` ((V 1 `I` V 2) `I` V 2)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (N (V 1) `I` V 2)]),Dual ([],[(V 1 `I` V 2) `I` V 2],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),N (V 1)],[],[]),Dual ([],[(V 1 `I` V 2) `I` V 2],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (V 1 `I` V 2)]),Dual ([V 2],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (V 1 `I` V 2)]),Dual ([V 2],[],[])],[(Dual ([],[],[N (V 1 `I` V 2)]),Dual ([V 2],[],[]))]), 
subForest = []}]}]}]}]}
-}

-- (((p v q) v r) -> (p v (q v r))) v s
h3 = proofDualtestx ((((V 1 `D` V 2) `D` V 3) `I` (V 1 `D` (V 2 `D` V 3))) `D` V 4)

{-
Node {rootLabel = ([Dual ([],[(((V 1 `D` V 2) `D` V 3) `I` (V 1 `D` (V 2 `D` V 3))) `D` V 4],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[((V 1 `D` V 2) `D` V 3) `I` (V 1 `D` (V 2 `D` V 3))],[]),Dual ([V 4],[],[])],[]),
subForest = [Node {rootLabel = ([Dual ([],[],[N ((V 1 `D` V 2) `D` V 3)]),Dual ([],[V 1 `D` (V 2 `D` V 3)],[]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 3)],[],[N (V 1 `D` V 2)]),Dual ([],[V 1 `D` (V 2 `D` V 3)],[]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),N (V 1),N (V 3)],[],[]),Dual ([],[V 1 `D` (V 2 `D` V 3)],[]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([V 1],[],[]),Dual ([],[V 2 `D` V 3],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([V 1],[],[]),Dual ([],[V 2 `D` V 3],[])],[(Dual ([V 1],[],[]),Dual ([],[V 2 `D` V 3],[]))]), 
 subForest = []}]}]}]}]}]}]}
-}

-- (p -> q) -> ((r & p) -> (r & q) v s)
h4 = proofDualtestx ((V 1 `I` V 2) `I` ((V 3 `A` V 1) `I` (V 3 `A` V 2)) `D` V 4)

{-
Node {rootLabel = ([Dual ([],[((V 1 `I` V 2) `I` ((V 3 `A` V 1) `I` (V 3 `A` V 2))) `D` V 4],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[(V 1 `I` V 2) `I` ((V 3 `A` V 1) `I` (V 3 `A` V 2))],[]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (V 1 `I` V 2)]),Dual ([],[(V 3 `A` V 1) `I` (V 3 `A` V 2)],[]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),V 1],[],[]),Dual ([],[(V 3 `A` V 1) `I` (V 3 `A` V 2)],[]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),V 1],[],[]),Dual ([],[N (V 3 `A` V 1)],[]),Dual ([],[],[V 3 `A` V 2]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[],[V 3 `A` V 2])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[],[V 3 `A` V 2])],[(Dual ([N (V 1)],[],[]),Dual ([],[],[V 3 `A` V 2]))]), 
subForest = []}]}]}]}]}]}]}
-}

-- ((p -> q) -> ((p -> ~q) -> ~p)) v s
h5 = proofDualtestx (((V 1 `I` V 2) `I` ((V 1 `I` N (V 2)) `I` N (V 1))) `D` V 4)
{-
Node {rootLabel = ([Dual ([],[((V 1 `I` V 2) `I` ((V 1 `I` N (V 2)) `I` N (V 1))) `D` V 4],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[(V 1 `I` V 2) `I` ((V 1 `I` N (V 2)) `I` N (V 1))],[]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (V 1 `I` V 2)]),Dual ([],[(V 1 `I` N (V 2)) `I` N (V 1)],[]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),V 1],[],[]),Dual ([],[(V 1 `I` N (V 2)) `I` N (V 1)],[]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (V 1 `I` N (V 2))]),Dual ([N (V 1)],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (V 1 `I` N (V 2))]),Dual ([N (V 1)],[],[])],[(Dual ([],[],[N (V 1 `I` N (V 2))]),Dual ([N (V 1)],[],[]))]), 
subForest = []}]}]}]}]}]}
-}

-- ((p & q) & r) -> (p & (q & r)) v s
h6 = proofDualtestx (((V 1 `A` V 2) `A` V 3) `I` (V 1 `A` (V 2 `A` V 3)) `D` V 5)

{-
Node {rootLabel = ([Dual ([],[(((V 1 `A` V 2) `A` V 3) `I` (V 1 `A` (V 2 `A` V 3))) `D` V 5],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[((V 1 `A` V 2) `A` V 3) `I` (V 1 `A` (V 2 `A` V 3))],[]),Dual ([V 5],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[N ((V 1 `A` V 2) `A` V 3)],[]),Dual ([],[],[V 1 `A` (V 2 `A` V 3)]),Dual ([V 5],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[N (V 1 `A` V 2)],[]),Dual ([N (V 3)],[],[]),Dual ([],[],[V 1 `A` (V 2 `A` V 3)]),Dual ([V 5],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[]),Dual ([],[],[V 1 `A` (V 2 `A` V 3)]),Dual ([V 5],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([V 1],[],[V 2 `A` V 3])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[V 2 `A` V 3]),Dual ([N (V 1)],[],[]),Dual ([V 1],[],[V 2 `A` V 3])],[(Dual ([N (V 1)],[],[]),Dual ([V 1],[],[V 2 `A` V 3]))]), 
subForest = []}]}]}]}]}]}]}
-}

b = resolution_poss2 (Node {rootLabel = ([Dual ([],[(((V 1 `A` V 2) `A` V 3) `I` (V 1 `A` (V 2 `A` V 3))) `D` V 5],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[((V 1 `A` V 2) `A` V 3) `I` (V 1 `A` (V 2 `A` V 3))],[]),Dual ([V 5],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[N ((V 1 `A` V 2) `A` V 3)],[]),Dual ([],[],[V 1 `A` (V 2 `A` V 3)]),Dual ([V 5],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[N (V 1 `A` V 2)],[]),Dual ([N (V 3)],[],[]),Dual ([],[],[V 1 `A` (V 2 `A` V 3)]),Dual ([V 5],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[]),Dual ([],[],[V 1 `A` (V 2 `A` V 3)]),Dual ([V 5],[],[])],[]), 
subForest = []}]}]}]}]})

s = dualprooftree (Node {rootLabel = ([Dual ([],[(((V 1 `A` V 2) `A` V 3) `I` (V 1 `A` (V 2 `A` V 3))) `D` V 5],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[((V 1 `A` V 2) `A` V 3) `I` (V 1 `A` (V 2 `A` V 3))],[]),Dual ([V 5],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[N ((V 1 `A` V 2) `A` V 3)],[]),Dual ([],[],[V 1 `A` (V 2 `A` V 3)]),Dual ([V 5],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[N (V 1 `A` V 2)],[]),Dual ([N (V 3)],[],[]),Dual ([],[],[V 1 `A` (V 2 `A` V 3)]),Dual ([V 5],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[]),Dual ([],[],[V 1 `A` (V 2 `A` V 3)]),Dual ([V 5],[],[])],[]), 
subForest = []}]}]}]}]})

w = cleanseq [Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[]),Dual ([],[],[V 1 `A` (V 2 `A` V 3)]),Dual ([V 5],[],[])]

-- (p v (q & r) -> (p v q) & (p v r)) v s
h7 = proofDualtestx ((V 1 `D` (V 2 `A` V 3)) `I` ((V 1 `D` V 2) `A` (V 1 `D` V 3)) `D` V 4)

{-
Node {rootLabel = ([Dual ([],[((V 1 `D` (V 2 `A` V 3)) `I` ((V 1 `D` V 2) `A` (V 1 `D` V 3))) `D` V 4],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[(V 1 `D` (V 2 `A` V 3)) `I` ((V 1 `D` V 2) `A` (V 1 `D` V 3))],[]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (V 1 `D` (V 2 `A` V 3))]),Dual ([],[],[(V 1 `D` V 2) `A` (V 1 `D` V 3)]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[N (V 2 `A` V 3)],[]),Dual ([],[],[(V 1 `D` V 2) `A` (V 1 `D` V 3)]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),N (V 1)],[],[]),Dual ([N (V 3),N (V 1)],[],[]),Dual ([],[],[(V 1 `D` V 2) `A` (V 1 `D` V 3)]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),N (V 1)],[],[]),Dual ([N (V 3),N (V 1)],[],[]),Dual ([],[V 1 `D` V 3,V 1 `D` V 2],[]),Dual ([V 4],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([V 1],[V 1 `D` V 2],[]),Dual ([V 3],[V 1 `D` V 2],[]),Dual ([N (V 3),N (V 1)],[],[]),Dual ([V 3],[V 1 `D` V 2],[]),Dual ([N (V 3),N (V 1)],[],[]),Dual ([V 1],[V 1 `D` V 2],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[V 1 `D` V 2],[]),Dual ([N (V 3),N (V 1)],[],[]),Dual ([V 1],[V 1 `D` V 2],[]),Dual ([V 3],[V 1 `D` V 2],[])],[(Dual ([V 3],[V 1 `D` V 2],[]),Dual ([N (V 3),N (V 1)],[],[]))]), 
subForest = []}]}]}]}]}]}]}]}
-}

-- ~(p & ~p) v q
h8 = proofDualtestx (N(V 1 `A` N(V 1)) `D` V 2)

{-
Node {rootLabel = ([Dual ([],[N (V 1 `A` N (V 1)) `D` V 2],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[N (V 1 `A` N (V 1))],[]),Dual ([V 2],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[],[ N(N (V 1))]),Dual ([V 2],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([V 1],[],[]),Dual ([V 2],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([V 2],[],[]),Dual ([],[],[]),Dual ([N (V 1)],[],[]),Dual ([V 1],[],[])],[(Dual ([V 1],[],[]),Dual ([V 2],[],[]))]), 
subForest = []}]}]}]}]}
-}

-- ~(p v q) -> ~p & ~q
h9 = proofDualtestx (N(V 1 `D` V 2) `I` (N(V 1) `A` N(V 2)) `D` V 3)
{-
Node {rootLabel = ([Dual ([],[(N (V 1 `D` V 2) `I` (N (V 1) `A` N (V 2))) `D` V 3],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[N (V 1 `D` V 2) `I` (N (V 1) `A` N (V 2))],[]),Dual ([V 3],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (N (V 1 `D` V 2))]),Dual ([],[],[N (V 1) `A` N (V 2)]),Dual ([V 3],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[V 1 `D` V 2],[]),Dual ([],[],[N (V 1) `A` N (V 2)]),Dual ([V 3],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([V 1],[],[]),Dual ([V 2],[],[]),Dual ([],[],[N (V 1) `A` N (V 2)]),Dual ([V 3],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([V 1],[],[]),Dual ([V 2],[],[]),Dual ([N (V 2),N (V 1)],[],[]),Dual ([V 3],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([V 3],[],[]),Dual ([N (V 2),N (V 1)],[],[]),Dual ([V 1],[],[]),Dual ([V 2],[],[])],[(Dual ([N (V 2),N (V 1)],[],[]),Dual ([V 3],[],[]))]), 
subForest = []}]}]}]}]}]}]}
-}

-- (p -> q) -> ((~p -> q) -> q) v r
h10 = proofDualtestx ((V 1 `I` V 2) `I` ((N(V 1) `I` V 2) `I` V 2) `D` V 3)
{-
Node {rootLabel = ([Dual ([],[((V 1 `I` V 2) `I` ((N (V 1) `I` V 2) `I` V 2)) `D` V 3],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[(V 1 `I` V 2) `I` ((N (V 1) `I` V 2) `I` V 2)],[]),Dual ([V 3],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (V 1 `I` V 2)]),Dual ([],[(N (V 1) `I` V 2) `I` V 2],[]),Dual ([V 3],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),V 1],[],[]),Dual ([],[(N (V 1) `I` V 2) `I` V 2],[]),Dual ([V 3],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (N (V 1) `I` V 2)]),Dual ([V 2],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (N (V 1) `I` V 2)]),Dual ([V 2],[],[])],[(Dual ([],[],[N (N (V 1) `I` V 2)]),Dual ([V 2],[],[]))]), 
subForest = []}]}]}]}]}]}
-}
-------------
-- from rest_test.txt

--(p -> (p -> q)) -> (~(p -> (~(~r -> s) -> (r -> t))) -> q)
h11 = proofDualtestx ((V 1 `I` (V 1 `I` V 2)) `I` (N(V 1 `I` (N(N(V 3) `I` V 4) `I` (V 3 `I` V 5))) `I` V 3))

--(p -> ~(q -> ~r)) -> (~(p -> q) -> ((q -> ~s) -> ~t))
{-
Node {rootLabel = ([Dual ([],[(V 1 `I` (V 1 `I` V 2)) `I` (N (V 1 `I` (N (N (V 3) `I` V 4) `I` (V 3 `I` V 5))) `I` V 3)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (V 1 `I` (V 1 `I` V 2))]),Dual ([],[N (V 1 `I` (N (N (V 3) `I` V 4) `I` (V 3 `I` V 5))) `I` V 3],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([V 1],[],[N (V 1 `I` V 2)]),Dual ([],[N (V 1 `I` (N (N (V 3) `I` V 4) `I` (V 
3 `I` V 5))) `I` V 3],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),V 1,V 1],[],[]),Dual ([],[N (V 1 `I` (N (N (V 3) `I` V 4) `I` (V 3 `I` V 5))) `I` V 3],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),V 1,V 1],[],[]),Dual ([],[],[N (N (V 1 `I` (N (N (V 3) `I` V 4) `I` (V 3 `I` V 5))))]),Dual ([V 3],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),V 1,V 1],[],[]),Dual ([],[V 1 `I` (N (N (V 3) `I` V 4) `I` (V 3 `I` V 5))],[]),Dual ([V 3],[],[])],[]), subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[N (N (V 3) `I` V 4) `I` (V 3 `I` V 5)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[N (N (V 3) `I` V 4) `I` (V 3 `I` V 5)],[])],[(Dual ([N (V 1)],[],[]),Dual ([],[N (N (V 3) `I` V 4) `I` (V 3 `I` V 5)],[]))]), 
subForest = []}]}]}]}]}]}]}]}
-}

--((p -> q) -> r) -> ((~(s -> p) -> (p -> (r -> t))) -> (q -> r))
h12 = proofDualtestx (((V 1 `I` V 2) `I` V 3) `I` ((N(V 4 `I` V 1) `I` (V 1 `I` (V 3 `I` V 5))) `I` (V 2 `I` V 3)))

{-
Node {rootLabel = ([Dual ([],[((V 1 `I` V 2) `I` V 3) `I` ((N (V 4 `I` V 1) `I` (V 1 `I` (V 3 `I` V 5))) `I` (V 2 `I` V 3))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N ((V 1 `I` V 2) `I` V 3)]),Dual ([],[(N (V 4 `I` V 1) `I` (V 1 `I` (V 3 `I` V 5))) `I` (V 2 `I` V 3)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 3)],[V 1 `I` V 2],[]),Dual ([],[(N (V 4 `I` V 1) `I` (V 1 `I` (V 3 `I` V 5))) `I` (V 2 `I` V 3)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1),N (V 3)],[],[]),Dual ([V 2,N (V 3)],[],[]),Dual ([],[(N (V 4 `I` V 1) `I` (V 1 `I` (V 3 `I` V 5))) `I` (V 2 `I` V 3)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1),N (V 3)],[],[]),Dual ([V 2,N (V 3)],[],[]),Dual ([],[],[N (N (V 4 `I` V 1) `I` (V 1 `I` (V 3 `I` V 5)))]),Dual ([],[V 2 `I` V 3],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1),N (V 3)],[],[]),Dual ([V 2,N (V 3)],[],[]),Dual ([],[],[N (V 1 `I` (V 3 `I` V 5)),N (V 4 `I` V 1)]),Dual ([],[V 2 `I` V 3],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([V 1],[],[N (V 3 `I` V 5),N (V 4 `I` V 1)]),Dual ([],[V 2 `I` V 3],[]),Dual ([N (V 1),N (V 3)],[],[]),Dual ([],[V 2 `I` V 3],[]),Dual ([N (V 1),N (V 3)],[],[]),Dual ([V 1],[],[N (V 3 `I` V 5),N (V 4 `I` V 1)])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 3)],[],[N (V 3 `I` V 5),N (V 4 `I` V 1)]),Dual ([N (V 1),N (V 3)],[],[]),Dual ([V 1],[],[N (V 3 `I` V 5),N (V 4 `I` V 1)]),Dual ([],[V 2 `I` V 3],[])],[(Dual ([],[V 2 `I` V 3],[]),Dual ([N (V 1),N (V 3)],[],[]))]), 
subForest = []}]}]}]}]}]}]}]}
-}

--p -> (q -> (r -> (((s -> p) -> (r -> s)) -> (~(t -> r) -> p))))
h13 = proofDualtestx (V 1 `I` (V 2 `I` (V 3 `I` (((V 4 `I` V 1) `I` (V 3 `I` V 4)) `I` (N(V 5 `I` V 3) `I` V 1)))))

{-
Node {rootLabel = ([Dual ([],[V 1 `I` (V 2 `I` (V 3 `I` (((V 4 `I` V 1) `I` (V 3 `I` V 4)) `I` (N (V 5 `I` V 3) `I` V 1))))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[V 2 `I` (V 3 `I` (((V 4 `I` V 1) `I` (V 3 `I` V 4)) `I` (N (V 5 `I` V 3) `I` V 1)))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([],[V 3 `I` (((V 4 `I` V 1) `I` (V 3 `I` V 4)) `I` (N (V 5 `I` V 3) `I` V 1))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[]),Dual ([],[((V 4 `I` V 1) `I` (V 3 `I` V 4)) `I` (N (V 5 `I` V 3) `I` V 1)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[]),Dual ([],[],[N ((V 4 `I` V 1) `I` (V 3 `I` V 
4))]),Dual ([],[N (V 5 `I` V 3) `I` V 1],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[]),Dual ([],[V 4 `I` V 1],[N (V 3 `I` V 4)]),Dual ([],[N (V 5 `I` V 3) `I` V 1],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 3)],[],[]),Dual ([],[N (V 5 `I` V 3) `I` V 1],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 3)],[],[]),Dual 
([],[N (V 5 `I` V 3) `I` V 1],[])],[(Dual ([N (V 3)],[],[]),Dual ([],[N (V 5 `I` V 3) `I` V 1],[]))]), 
subForest = []}]}]}]}]}]}]}]}
-}

--((p -> (q -> (r -> (~p -> r)))) -> ~~s) -> (t -> (s -> s))
h14 = proofDualtestx (((V 1 `I` (V 2 `I` (V 3 `I` (N(V 1) `I` V 3)))) `I` N(N(V 4))) `I` (V 5 `I` (V 4 `I` V 4)))

{-
Node {rootLabel = ([Dual ([],[((V 1 `I` (V 2 `I` (V 3 `I` (N (V 1) `I` V 3)))) `I` N (N (V 4))) `I` (V 5 `I` (V 4 `I` V 4))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N ((V 1 `I` (V 2 `I` (V 3 `I` (N (V 1) `I` V 3)))) `I` N (N (V 4)))]),Dual ([],[V 5 `I` (V 4 `I` V 4)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[V 1 `I` (V 2 `I` (V 3 `I` (N (V 1) `I` V 3)))],[N (N (N (V 4)))]),Dual ([],[V 5 `I` (V 4 `I` V 4)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 4)],[V 1 `I` (V 2 `I` (V 3 `I` (N (V 1) `I` V 3)))],[]),Dual ([],[V 5 `I` (V 4 `I` V 4)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1),N (V 4)],[],[]),Dual ([N (V 4)],[V 2 `I` (V 3 `I` (N (V 1) `I` V 3))],[]),Dual ([],[V 5 `I` (V 4 `I` V 4)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1),N (V 4)],[],[]),Dual ([N (V 2),N (V 4)],[],[]),Dual ([N (V 4)],[V 3 `I` (N (V 1) `I` V 3)],[]),Dual ([],[V 5 `I` (V 4 `I` V 4)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1),N (V 4)],[],[]),Dual ([N (V 2),N (V 4)],[],[]),Dual ([N (V 3),N (V 4)],[],[]),Dual ([N (V 4)],[N (V 1) `I` V 3],[]),Dual ([],[V 5 `I` (V 4 `I` V 4)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([V 3,N (V 4)],[],[]),Dual ([],[V 5 `I` (V 4 `I` V 4)],[]),Dual ([N (V 4)],[],[N (N (V 1))]),Dual ([],[V 5 `I` (V 4 `I` V 4)],[]),Dual ([N (V 4)],[],[N (N (V 1))]),Dual ([V 3,N (V 4)],[],[]),Dual ([N (V 3),N (V 4)],[],[]),Dual ([],[V 5 `I` (V 4 `I` V 4)],[]),Dual ([N (V 3),N (V 4)],[],[]),Dual ([V 3,N (V 4)],[],[]),Dual ([N (V 3),N (V 4)],[],[]),Dual ([N (V 4)],[],[N (N (V 1))])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 4)],[],[]),Dual ([N (V 3),N (V 4)],[],[]),Dual ([N (V 4)],[],[N (N (V 1))]),Dual ([V 3,N (V 4)],[],[]),Dual ([],[V 5 `I` (V 4 `I` V 4)],[])],[(Dual ([V 3,N (V 4)],[],[]),Dual ([N (V 3),N (V 
4)],[],[]))]), 
subForest = []}]}]}]}]}]}]}]}]}
-}

--(((p -> q) -> ~~((r -> ~(q -> s)) -> (p -> p))) -> t) -> t
h15 = proofDualtestx ((((V 1 `I` V 2) `I` N(N((V 3 `I` N(V 2 `I` V 4)) `I` (V 1 `I` V 1)))) `I` V 5) `I` V 5)

{-
Node {rootLabel = ([Dual ([],[(((V 1 `I` V 2) `I` N (N ((V 3 `I` N (V 2 `I` V 4)) `I` (V 1 `I` V 1)))) `I` V 5) `I` V 5],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (((V 1 `I` V 2) `I` N 
(N ((V 3 `I` N (V 2 `I` V 4)) `I` (V 1 `I` V 1)))) `I` V 5)]),Dual ([V 5],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 5)],[(V 1 `I` V 2) `I` N (N ((V 3 `I` N (V 2 `I` V 4)) `I` (V 1 `I` V 1)))],[]),Dual ([V 5],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[(V 1 `I` V 2) `I` N (N ((V 3 `I` N (V 2 `I` V 4)) `I` (V 1 `I` V 1)))],[]),Dual ([N (V 5)],[(V 1 `I` V 2) `I` N (N ((V 3 `I` N (V 2 `I` V 4)) `I` (V 1 `I` V 1)))],[]),Dual ([V 5],[],[])],[(Dual ([N (V 5)],[(V 1 `I` V 2) `I` N (N ((V 3 `I` N (V 2 `I` V 4)) `I` (V 1 `I` V 1)))],[]),Dual ([V 5],[],[]))]), 
subForest = []}]}]}]}
-}

--p -> (~(q -> (q -> ((~(p -> r) -> r) -> (s -> t)))) -> ~t)
h16 = proofDualtestx (V 1 `I` (N(V 2 `I` (V 2 `I` ((N(V 1 `I` V 3) `I` V 3) `I` (V 4 `I` V 5)))) `I` N(V 5)))
{-
Node {rootLabel = ([Dual ([],[V 1 `I` (N (V 2 `I` (V 2 `I` ((N (V 1 `I` V 3) `I` V 3) `I` (V 4 `I` V 5)))) `I` N (V 5))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[N (V 2 `I` (V 2 `I` ((N (V 1 `I` V 3) `I` V 3) `I` (V 4 `I` V 5)))) `I` N (V 5)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[],[N (N (V 2 `I` (V 2 `I` ((N (V 1 `I` V 3) `I` V 3) `I` (V 4 `I` V 5)))))]),Dual ([N (V 5)],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[V 2 `I` (V 2 `I` ((N (V 1 `I` V 3) `I` V 3) `I` (V 4 `I` V 5)))],[]),Dual ([N (V 5)],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([],[V 2 `I` ((N (V 1 `I` V 3) `I` V 3) `I` (V 4 `I` V 5))],[]),Dual ([N (V 5)],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 2)],[],[]),Dual ([],[(N (V 1 `I` V 3) `I` V 3) `I` (V 4 `I` V 5)],[]),Dual ([N (V 5)],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 2)],[],[]),Dual ([],[],[N (N (V 1 `I` V 3) `I` V 3)]),Dual ([],[V 4 `I` V 5],[]),Dual ([N (V 5)],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[N (V 1 `I` V 3)]),Dual ([],[V 4 `I` V 5],[]),Dual ([N (V 5)],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2)],[],[]),Dual ([],[V 4 `I` V 5],[]),Dual ([N (V 2)],[],[]),Dual ([],[V 4 `I` V 5],[]),Dual ([N (V 1)],[],[]),Dual ([],[V 4 `I` V 5],[]),Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([],[V 4 `I` V 5],[])],[(Dual ([],[V 4 `I` V 5],[]),Dual ([N (V 1)],[],[]))]), 
subForest = []}]}]}]}]}]}]}]}]}]}
-}

--(p -> q) -> (~(p -> (r -> ~~s)) -> (t -> ((r -> t) -> r)))
h17 = proofDualtestx ((V 1 `I` V 2) `I` (N(V 1 `I` (V 3 `I` N(N(V 4)))) `I` (V 5 `I` ((V 3 `I` V 5) `I` V 3))))

{-
Node {rootLabel = ([Dual ([],[(V 1 `I` V 2) `I` (N (V 1 `I` (V 3 `I` N (N (V 4)))) `I` (V 5 `I` ((V 3 `I` V 5) `I` V 3)))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[N (V 1 `I` V 2)]),Dual ([],[N (V 1 `I` (V 3 `I` N (N (V 4)))) `I` (V 5 `I` ((V 3 `I` V 5) `I` V 3))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),V 1],[],[]),Dual ([],[N (V 1 `I` (V 3 `I` N (N (V 4)))) `I` (V 5 `I` ((V 3 `I` V 5) `I` V 3))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),V 1],[],[]),Dual ([],[],[N (N (V 1 `I` (V 3 `I` N (N (V 4)))))]),Dual ([],[V 5 `I` ((V 3 `I` V 5) `I` V 3)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2),V 1],[],[]),Dual ([],[V 1 `I` (V 3 `I` N (N (V 4)))],[]),Dual ([],[V 5 `I` ((V 3 `I` V 5) `I` V 3)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[V 3 `I` N (N (V 4))],[]),Dual ([],[V 5 `I` ((V 3 `I` V 5) `I` V 3)],[]),Dual ([N (V 1)],[],[]),Dual ([],[V 5 `I` ((V 3 `I` V 5) `I` V 3)],[]),Dual ([N (V 1)],[],[]),Dual ([],[V 3 `I` N (N (V 4))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[V 3 `I` 
N (N (V 4))],[]),Dual ([],[V 5 `I` ((V 3 `I` V 5) `I` V 3)],[])],[(Dual ([],[V 5 `I` ((V 3 `I` V 5) `I` V 3)],[]),Dual ([N (V 1)],[],[]))]), 
subForest = []}]}]}]}]}]}]}
-}

--p -> (q -> (r -> ((s -> q) -> ((~(p -> p) -> t) -> (p -> r)))))
h18 = proofDualtestx (V 1 `I` (V 2 `I` (V 3 `I` ((V 4 `I` V 2) `I` ((N(V 1 `I` V 1) `I` V 5) `I` (V 1 `I` V 3))))))

{-
Node {rootLabel = ([Dual ([],[V 1 `I` (V 2 `I` (V 3 `I` ((V 4 `I` V 2) `I` ((N (V 1 `I` V 1) `I` V 5) `I` (V 1 `I` V 3)))))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[V 2 `I` (V 3 `I` ((V 4 `I` V 2) `I` ((N (V 1 `I` V 1) `I` V 5) `I` (V 1 `I` V 3))))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([],[V 3 `I` ((V 4 `I` V 2) `I` ((N (V 1 `I` V 1) `I` V 5) `I` (V 1 `I` V 3)))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[]),Dual ([],[(V 4 `I` V 2) `I` ((N (V 1 `I` V 1) `I` V 5) `I` (V 1 `I` V 3))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[]),Dual ([],[],[N (V 4 `I` V 2)]),Dual ([],[(N (V 1 `I` V 1) `I` V 5) `I` (V 1 `I` V 3)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[]),Dual ([N (V 2),V 4],[],[]),Dual ([],[(N (V 1 `I` V 1) `I` V 5) `I` (V 1 `I` V 3)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[]),Dual ([N (V 2),V 4],[],[]),Dual ([],[],[N (N (V 1 `I` V 1) `I` V 5)]),Dual ([],[V 1 `I` V 3],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([N (V 3)],[],[]),Dual ([N (V 2),V 4],[],[]),Dual ([N (V 5)],[],[N (V 1 `I` V 1)]),Dual ([],[V 1 `I` V 3],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2)],[],[]),Dual ([],[V 1 `I` V 3],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 2)],[],[]),Dual ([],[V 1 `I` V 3],[])],[(Dual ([N (V 2)],[],[]),Dual ([],[V 1 `I` V 3],[]))]), 
subForest = []}]}]}]}]}]}]}]}]}]}
-}

--p -> (q -> ~~((r -> ((s -> s) -> ~t)) -> ~~(p -> p)))
h19 = proofDualtestx (V 1 `I` (V 2 `I` N(N((V 3 `I` ((V 4 `I` V 4) `I` N(V 5))) `I` N(N(V 1 `I` V 1))))))

{-
Node {rootLabel = ([Dual ([],[V 1 `I` (V 2 `I` N (N ((V 3 `I` ((V 4 `I` V 4) `I` N (V 5))) `I` N (N (V 1 `I` V 1)))))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[V 2 `I` N (N ((V 3 `I` ((V 4 `I` V 4) `I` N (V 5))) `I` N (N (V 1 `I` V 1))))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([],[],[N (N ((V 3 `I` ((V 4 `I` V 4) `I` N (V 5))) `I` N (N (V 1 `I` V 1))))])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([],[(V 3 `I` ((V 4 `I` V 4) `I` N (V 5))) `I` N (N (V 1 `I` V 1))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([],[],[N (V 3 `I` ((V 4 `I` V 4) `I` N (V 5)))]),Dual ([],[],[N (N (V 1 `I` V 1))])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([V 3],[],[N ((V 4 `I` V 4) `I` N (V 5))]),Dual ([],[],[N (N (V 1 `I` V 1))])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([V 3],[V 4 `I` V 4],[N (N (V 5))]),Dual ([],[],[N (N (V 1 `I` V 1))])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2)],[],[]),Dual ([V 5,V 3],[V 4 `I` V 4],[]),Dual ([],[],[N (N (V 1 `I` V 1))])],[]), 
subForest = [Node {rootLabel = ([Dual ([V 4,V 5,V 3],[],[]),Dual ([],[],[N (N (V 1 `I` V 1))]),Dual ([N (V 4),V 5,V 3],[],[]),Dual ([],[],[N (N (V 1 `I` V 1))]),Dual ([N (V 4),V 5,V 3],[],[]),Dual ([V 4,V 5,V 3],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([V 5,V 3],[],[]),Dual ([N (V 4),V 5,V 3],[],[]),Dual ([V 4,V 5,V 3],[],[]),Dual ([],[],[N (N (V 1 `I` V 1))])],[(Dual ([],[],[N (N (V 1 `I` V 1))]),Dual ([N (V 4),V 5,V 3],[],[]))]), 
subForest = []}]}]}]}]}]}]}]}]}]}
-}

----TRUE - correct
--p -> (((q -> ~r) -> s) -> (~p -> (~(r -> t) -> (t -> s))))
hx = proofDualtestx (V 1 `I` (((V 2 `I` N(V 3)) `I` V 4) `I` (N(V 1) `I` (N(V 3 `I` V 5) `I` (V 5 `I` V 4)))))
{-
Node {rootLabel = ([Dual ([],[V 1 `I` (((V 2 `I` N (V 3)) `I` V 4) `I` (N (V 1) `I` (N (V 3 `I` V 5) `I` (V 5 `I` V 4))))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[((V 2 `I` N (V 3)) `I` V 4) `I` (N (V 1) `I` (N (V 3 `I` V 5) `I` (V 5 `I` V 4)))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([],[],[N ((V 2 `I` N (V 3)) `I` V 4)]),Dual ([],[N (V 1) `I` (N (V 3 `I` V 5) `I` (V 5 `I` V 4))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 4)],[V 2 `I` N (V 3)],[]),Dual ([],[N (V 1) `I` (N (V 3 `I` V 5) `I` (V 5 `I` V 4))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2),N (V 4)],[],[]),Dual ([N (V 3),N (V 4)],[],[]),Dual ([],[N (V 1) `I` (N (V 3 `I` V 5) `I` (V 5 `I` V 4))],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([N (V 1)],[],[]),Dual ([N (V 2),N (V 4)],[],[]),Dual ([N (V 3),N (V 4)],[],[]),Dual ([],[],[N (N (V 1))]),Dual ([],[N (V 3 `I` V 5) `I` (V 5 `I` V 4)],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([V 1],[],[]),Dual ([],[N (V 3 `I` V 5) `I` (V 5 `I` V 4)],[]),Dual ([N (V 1)],[],[]),Dual ([],[N (V 3 `I` V 5) `I` (V 5 `I` V 4)],[]),Dual ([N (V 1)],[],[]),Dual ([V 1],[],[])],[]), 
subForest = [Node {rootLabel = ([Dual ([],[],[]),Dual ([N (V 1)],[],[]),Dual ([V 1],[],[]),Dual ([],[N (V 3 `I` V 5) `I` (V 5 `I` V 4)],[])],[(Dual ([],[N (V 3 `I` V 5) `I` (V 5 `I` V 4)],[]),Dual ([N (V 1)],[],[]))]), 
subForest = []}]}]}]}]}]}]}]}
-}
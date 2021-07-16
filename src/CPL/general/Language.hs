module Language where

import Data.Tree

-- | Inductive definition of Formula type
data For =   Verum
            | V Int
            | N For       -- classical negation
            | U For       -- paraconsistent negation
            | C For       -- "behaves classically"
            | For `E` For
            | For `I` For
            | For `A` For
            | For `D` For deriving(Eq,Read,Show,Ord)

-- | Binding rules for propositional connectives
--infix 9 `V`
--infixr 8 `N`
--infixr 7 `A`
--infixr 7 `D`
--infixr 6 `I`
--infix 6 `E`

-- | Maybe formula is a Just formula or Nothing
type MF = Maybe For

-- | Rose trees labelled by lists of maybe formulas
type MT = Tree [MF]

newtype CanSeq = Can ([For], [For], [For]) deriving(Eq,Read,Show,Ord)
-- literals, alpha, beta

newtype DualSeq = Dual ([For], [For], [For]) deriving(Eq,Read,Show,Ord)
-- literals, beta, alpha

data Cal = CanonicalRight | DualRight

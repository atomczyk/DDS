module AuxiliaryTrees
{-
  (
    actOnLeaves'
  , fixpoint
  , numBranch
  , branches
  , numberOfBranches
  , leaves
  , numberOfVertices
  , depthOfTree
  )
-}
where

import Language
import Data.Tree


-- | A general function which apply a rule of type `[MF] -> For -> MF` to
-- all leaves of a tree. That rule may be classical or not.
actOnLeaves' :: For -> ([MF] -> For -> MF) -> For -> MT -> MT
actOnLeaves' goal f for tree = case tree of
  Node x []     -> case ((Just goal) `elem` x) of
                    True  -> Node x []
                    False -> case f x for of
                      Nothing -> Node x []
                      Just y  -> Node x [Node (x ++ [f x for]) []]
  Node x (y:ys) -> Node x (map (actOnLeaves' goal f for) (y:ys))

-- | Check whether a given argument is a fixpoint of a function.
fixpoint :: Eq a => (a -> a) -> a -> Bool
fixpoint f x = (f x == x)

-- | Return the number of branches in a tree.
numberOfBranches :: Tree a -> Int
numberOfBranches tree = length $ branches tree

depthOfTree :: Tree a -> Int
depthOfTree (Node x [])     = 1
depthOfTree (Node x (y:ys)) = 1 + maximum (map depthOfTree (y:ys))

b_depthOfTree :: Tree [MF] -> Int
b_depthOfTree (Node x [])       = numberOfLiterals x
b_depthOfTree (Node x (y:ys))   = maximum $ map (b_depthOfTree) (y:ys)

numberOfLiterals :: [MF] -> Int
numberOfLiterals []     = 0
numberOfLiterals (x:xs) = case x of
      Just (V _)      -> 1 + numberOfLiterals xs
      Just (N (V _))  -> 1 + numberOfLiterals xs
      _               -> numberOfLiterals xs


--numberOfLiterals [Just (V 1), Just (N (V 2)), Just (V 3)]

numberOfNodes :: Tree a -> Int
numberOfNodes (Node x [])      = 1
numberOfNodes (Node x (y:ys))  = 1 + sum (map numberOfNodes (y:ys))

-- | Return numbers of branches in a list of trees.
numBranch :: [MT] -> [Int]
numBranch []      = []
numBranch (x:xs)  = (numberOfBranches x): numBranch xs

-- | Generate all branches of a tree.
branches :: Tree a -> [[a]]
branches x = case x of
    Node x []   -> [[x]]
    Node x ys   -> map (x :) $ foldl (++) [] $ map branches ys

-- | Return the list of leaves of a given tree.
leaves :: Tree a -> [a]
leaves x = map last $ branches x

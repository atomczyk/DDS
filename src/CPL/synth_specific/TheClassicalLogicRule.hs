module TheClassicalLogicRule
  (
    synthesizeFor
  )
where

import Language

-- | The rule for classical logic.
synthesizeFor :: [MF] -> For -> MF
synthesizeFor ys (N (E l r))
  | Just (N (E l r)) `elem` ys                                                           = Nothing
  | (Just l) `elem` ys && (Just (N r)) `elem` ys && not (Just (N (E l r)) `elem` ys)     = Just (N (E l r))
  | (Just (N l)) `elem` ys && (Just r) `elem` ys && not (Just (N (E l r)) `elem` ys)     = Just (N (E l r))
  | otherwise                                                                            = Nothing
synthesizeFor ys (E l r)
  | Just (E l r) `elem` ys                                                               = Nothing
  | ((Just l) `elem` ys && (Just r) `elem` ys) && (not (Just (E l r) `elem` ys))         = Just (E l r)
  | ((Just (N l)) `elem` ys && (Just (N r)) `elem` ys) && (not (Just (E l r) `elem` ys)) = Just (E l r)
  | otherwise                                                                            = Nothing
synthesizeFor ys (N (N x))
  | Just (N (N x)) `elem` ys                                                             = Nothing
  | ((Just x) `elem` ys) && not ((Just (N (N x))) `elem` ys)                             = Just (N (N x))
  | otherwise                                                                            = Nothing
synthesizeFor ys (I x y)
  | Just (I x y) `elem` ys                                                               = Nothing
  | (Just (N x)) `elem` ys || (Just y) `elem` ys                                         = Just (I x y)
  | otherwise                                                                            = Nothing
synthesizeFor ys (N (I x y))
  | Just (N (I x y)) `elem` ys                                                           = Nothing
  | (Just x) `elem` ys && (Just (N y)) `elem` ys                                         = Just (N (I x y))
  | otherwise                                                                            = Nothing
synthesizeFor ys (A x y)
  | Just (A x y) `elem` ys                                                               = Nothing
  | (Just x) `elem` ys && (Just y) `elem` ys                                             = Just (A x y)
  | otherwise                                                                            = Nothing
synthesizeFor ys (N (A x y))
  | Just (N (A x y)) `elem` ys                                                           = Nothing
  | (Just (N x)) `elem` ys || (Just (N y)) `elem` ys                                     = Just (N (A x y))
  | otherwise                                                                            = Nothing
synthesizeFor ys (D x y)
  | Just (D x y) `elem` ys                                                               = Nothing
  | (Just x) `elem` ys || (Just y) `elem` ys                                             = Just (D x y)
  | otherwise                                                                            = Nothing
synthesizeFor ys (N (D x y))
  | Just (N (D x y)) `elem` ys                                                           = Nothing
  | (Just (N x)) `elem` ys && (Just (N y)) `elem` ys                                     = Just (N (D x y))
  | otherwise                                                                            = Nothing

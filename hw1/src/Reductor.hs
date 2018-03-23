module Reductor
  ( normilize
  ) where

  import Simplifier (Expression(..))
  import Data.List
  import qualified Data.Map.Strict as Map

  type Matcher = Map.Map String String

  normilize :: Expression -> Expression
  normilize = normilizeI 0
    where
      normilizeI :: Integer -> Expression -> Expression
      normilizeI i e = case betaReduction i e of
        Just x  -> normilizeI (fst x) (snd x)
        Nothing -> e

  betaReduction :: Integer -> Expression -> Maybe (Integer, Expression)
  betaReduction i (Application l r) = case l of
    Lambda v e -> do
      let col = delete v $ intersect (getVars l) (getVars r)
      let (j, m) = getMatcher i col
      return (j, substitute v (rename m r) e)
    _ -> case betaReduction i l of
      Just x  -> return (fst x, Application (snd x) r)
      Nothing -> case betaReduction i r of
        Just y  -> return (fst y, Application l (snd y))
        Nothing -> Nothing
  betaReduction i (Lambda v e) = case betaReduction i e of
    Just x  -> return (fst x, Lambda v (snd x))
    Nothing -> Nothing
  betaReduction _ (Variable _) = Nothing

  substitute :: String -> Expression -> Expression -> Expression
  substitute s x (Application l r) = Application (substitute s x l) (substitute s x r)
  substitute s x (Lambda      v e) = if s == v
    then Lambda v e
    else Lambda v (substitute s x e)
  substitute s x (Variable    v  ) = if s == v
    then x
    else Variable v

  getVars :: Expression -> [String]
  getVars (Application l r) = getVars l ++ getVars r
  getVars (Lambda      v e) = getVars e ++ [v]
  getVars (Variable    _  ) = []

  getMatcher :: Integer -> [String] -> (Integer, Matcher)
  getMatcher i (x:xs) = let (j, m) = getMatcher i xs in (j + 1, Map.insert x (getVar j) m)
  getMatcher i [] = (i, Map.empty)

  getVar :: Integer -> String
  getVar i = "rv_" ++ (show i)

  rename :: Matcher -> Expression -> Expression
  rename m (Application l r) = Application (rename m l) (rename m r)
  rename m (Lambda      v e) = Lambda (Map.findWithDefault v v m) (rename m e)
  rename m (Variable    v  ) = Variable $ Map.findWithDefault v v m

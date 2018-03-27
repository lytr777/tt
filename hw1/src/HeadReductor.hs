module HeadReductor
  ( normilize
  ) where

  import Simplifier (Expression(..))
  import Data.List (elem, (\\))

  type Scope = [String]

  normilize :: Expression -> Expression
  normilize = reduction []

  reduction :: Scope -> Expression -> Expression
  reduction sc (Lambda      v e) = Lambda v (reduction (v:sc) e)
  reduction sc (Application l r) = case headReduction [] l of
    Lambda v e -> reduction (v:sc) (substitute (v:sc) v r e)
    e          -> Application (reduction [] e) (reduction [] r)
  reduction _  v                 = v

  headReduction :: Scope -> Expression -> Expression
  headReduction sc (Lambda      v e) = Lambda v (headReduction (v:sc) e)
  headReduction sc (Application l r) = case headReduction sc l of
    Lambda v e -> headReduction (v:sc) (substitute (v:sc) v r e)
    e          -> Application e r
  headReduction _ v                 = v

  substitute :: Scope -> String -> Expression -> Expression -> Expression
  substitute sc s x (Application l r) = Application (substitute sc s x l) (substitute sc s x r)
  substitute sc s x (Lambda      v e) = do
    let vars = getFreeVars e
    if (s == v) || (not $ elem s vars)
      then Lambda v e
      else do
        let fv = getVar (sc ++ vars ++ (getFreeVars x)) 0
        Lambda fv (substitute (fv:sc) s x (substitute (fv:sc) v (Variable fv) e))
  substitute _  s x (Variable    v  ) = if s == v
    then x
    else Variable v

  getFreeVars :: Expression -> [String]
  getFreeVars (Application l r) = getFreeVars l ++ getFreeVars r
  getFreeVars (Lambda      v e) = getFreeVars e \\ [v]
  getFreeVars (Variable    v  ) = [v]

  getVar :: Scope -> Integer -> String
  getVar sc i = do
    let v = "rv_" ++ (show i)
    if elem ("rv_" ++ (show i)) sc
      then getVar sc (i + 1)
      else v

module HeadReductor
  ( normilize
  ) where

  import Control.Monad.State
  import Simplifier (Expression(..))
  import Data.List (elem, (\\))
  import qualified Data.Map.Strict as Map

  type Scope = [String]
  type Hash = Map.Map Expression Expression
  type HeadState = State Hash Expression

  normilize :: Expression -> Expression
  normilize e = fst $ runState (reduction [] e) Map.empty

  reduction :: Scope -> Expression -> HeadState
  reduction sc (Lambda      v e) = (Lambda v) <$> (reduction (v:sc) e)
  reduction sc (Application l r) = do
    hr <- headReduction [] l
    case hr of
      Lambda v e -> reduction (v:sc) (substitute (v:sc) v r e)
      e          -> Application <$> (reduction [] e) <*> (reduction [] r)
  reduction _  v                 = return v

  headReduction :: Scope -> Expression -> HeadState
  headReduction sc (Lambda      v e) = (Lambda v) <$> (headReduction (v:sc) e)
  headReduction sc (Application l r) = do
    hr <- headReduction sc l
    case hr of
      e@(Lambda v lb) -> do
        let key = Application e r
        val <- gets (Map.lookup key)
        case val of
          Just x  -> return x
          Nothing -> do
            result <- headReduction (v:sc) (substitute (v:sc) v r lb)
            modify (Map.insert key result)
            return result
      e               -> return $ Application e r
  headReduction _ v                 = return v

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

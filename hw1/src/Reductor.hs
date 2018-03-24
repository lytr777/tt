module Reductor
  ( normilize
  ) where

  import Control.Monad.State
  import Simplifier (Expression(..))
  import Data.List
  import qualified Data.Map.Strict as Map

  type Matcher = Map.Map String String
  type Memory = Map.Map Expression (Maybe Expression)
  type MemState = State Memory (Maybe (Integer, Expression))

  normilize :: Expression -> Expression
  normilize ex = case fst $ runState (normilizeI 0 0 ex) Map.empty of
    Just x  -> snd x
    Nothing -> ex
    where
      normilizeI :: Integer -> Integer -> Expression -> MemState
      normilizeI k i e = do
        may <- betaReduction i e
        case may of
          Just x  -> normilizeI (k+1) (fst x) (snd x)
          Nothing -> return $ return (0, e)

  betaReduction :: Integer -> Expression -> MemState
  betaReduction i (Application l r) = case l of
    Lambda v e -> do
      let col = delete v $ intersect (getVars l) (getVars r)
      let (j, m) = getMatcher i col
      return $ return (j, substitute v (rename m r) e)
    _ -> do
      mayL <- betaReduction i l
      case mayL of
        Just x  -> return $ return (fst x, Application (snd x) r)
        Nothing -> do
          mayR <- betaReduction i r
          case mayR of
            Just y  -> return $ return (fst y, Application l (snd y))
            Nothing -> return Nothing
  betaReduction i (Lambda v e) = do
    may <- betaReduction i e
    case may of
      Just x  -> return $ return (fst x, Lambda v (snd x))
      Nothing -> return Nothing
  betaReduction _ (Variable _) = return Nothing

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

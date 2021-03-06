module Resolver
  ( resolveSystem
  , insertType
  , Matcher
  ) where

  import Control.Applicative ((<|>))
  import Typer (TypeExpr (..))
  import qualified Data.Map.Strict as Map

  type Matcher = Map.Map String TypeExpr

  resolveSystem :: [TypeExpr] -> Maybe Matcher
  resolveSystem sys = do
    let fSys = filterSystem sys
    if checkSolved [] fSys
      then Just $ getMatcher fSys
      else if checkSolvable fSys
        then resolveSystem $ transforming fSys fSys
        else Nothing

  filterSystem :: [TypeExpr] -> [TypeExpr]
  filterSystem (x:xs) =  if elem x xs
    then filterSystem xs
    else x : (filterSystem xs)
  filterSystem [] = []

  checkSolved :: [TypeExpr] -> [TypeExpr] -> Bool
  checkSolved h (x:t) = (checkExpr (h ++ t) x) && (checkSolved (h ++ [x]) t)
  checkSolved _ [] = True

  checkSolvable :: [TypeExpr] -> Bool
  checkSolvable (x:xs) = case x of
    Equation l r -> case l of
      TypeVariable s -> (l == r) || (not $ elem s (getVars r))
      _              -> checkSolvable xs
    _            -> False
  checkSolvable [] = True

  transforming :: [TypeExpr] -> [TypeExpr] -> [TypeExpr]
  transforming list (x:xs) = (transformExpr list x) ++ (transforming list xs)
  transforming _ [] = []

  transformExpr :: [TypeExpr] -> TypeExpr -> [TypeExpr]
  transformExpr list (Equation l r) = do
    case (firstCase l r) <|> (secondCase l r) <|> (thirdCase l r) <|> (fourthCase list l r) of
      Just cr -> cr
      Nothing -> []
  transformExpr _ x = [x]

  firstCase :: TypeExpr -> TypeExpr -> Maybe [TypeExpr]
  firstCase l r = if l == r
    then Just $ []
    else Nothing

  secondCase :: TypeExpr -> TypeExpr -> Maybe [TypeExpr]
  secondCase (TypeVariable _) (TypeVariable _) = Nothing
  secondCase l r@(TypeVariable _) = Just $ [Equation r l]
  secondCase _ _  = Nothing

  thirdCase :: TypeExpr -> TypeExpr -> Maybe [TypeExpr]
  thirdCase l@(Implication _ _) r@(Implication _ _) = Just $ separation l r
  thirdCase _ _ = Nothing

  separation :: TypeExpr -> TypeExpr -> [TypeExpr]
  separation (Implication a b) (Implication c d) = (separation a c) ++ (separation b d)
  separation l r = [Equation l r]

  fourthCase :: [TypeExpr] -> TypeExpr -> TypeExpr -> Maybe [TypeExpr]
  fourthCase list l r = do
    let vars = (getVars l) ++ (getVars r)
    let p = (\k v -> (not $ (l == (TypeVariable k) && r == v)) && (elem k vars))
    let m = Map.filterWithKey p (getMatcher list)
    return [Equation (insertType l m) (insertType r m)]

  checkExpr :: [TypeExpr] -> TypeExpr -> Bool
  checkExpr list (Equation l r) = case l of
    TypeVariable s -> not $ elem s ((concat $ map getVars list) ++ getVars r)
    _              -> False
  checkExpr _ _ = False

  getVars :: TypeExpr -> [String]
  getVars (Equation    l r) = getVars l ++ getVars r
  getVars (Implication l r) = getVars l ++ getVars r
  getVars (TypeVariable  s) = [s]

  getMatcher :: [TypeExpr] -> Matcher
  getMatcher list = foldl addExpr Map.empty list
    where
      addExpr :: Matcher -> TypeExpr -> Matcher
      addExpr m (Equation l r) = case l of
        TypeVariable s -> Map.insert s r m
        _              -> m
      addExpr m _ = m

  insertType :: TypeExpr -> Matcher -> TypeExpr
  insertType (Equation    l r) m = Equation (insertType l m) (insertType r m)
  insertType (Implication l r) m = Implication (insertType l m) (insertType r m)
  insertType (TypeVariable  s) m = case Map.lookup s m of
    Just x  -> x
    Nothing -> TypeVariable s

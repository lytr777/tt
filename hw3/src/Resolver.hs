module Resolver
  ( resolveSystem
  , insertType
  , getVars
  , getFreeVars
  , Matcher
  , TypeExpr(..)
  ) where

  import Data.List ((\\))
  import Control.Applicative ((<|>))
  import qualified Data.Map.Strict as Map

  data TypeExpr = Implication  TypeExpr TypeExpr
                | Equation     TypeExpr TypeExpr
                | UniversalQ   String   TypeExpr
                | TypeVariable String

  instance Show TypeExpr where
    show (Equation    l r) = show l ++ " = " ++ show r
    show (Implication l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"
    show (UniversalQ  s e) = "@" ++ s ++ " . " ++ show e
    show (TypeVariable  s) = s

  instance Eq TypeExpr where
    (==) (Implication l1 r1) (Implication l2 r2) = (l1 == l2) && (r1 == r2)
    (==) (Equation    l1 r1) (Equation    l2 r2) = (l1 == l2) && (r1 == r2)
    (==) (UniversalQ  s1 e1) (UniversalQ  s2 e2) = (s1 == s2) && (e1 == e2)
    (==) (TypeVariable   s1) (TypeVariable   s2) = (s1 == s2)
    (==) _                   _                   = False

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
    return [Equation (insertType m l) (insertType m r)]

  checkExpr :: [TypeExpr] -> TypeExpr -> Bool
  checkExpr list (Equation l r) = case l of
    TypeVariable s -> not $ elem s ((concat $ map getVars list) ++ getVars r)
    _              -> False
  checkExpr _ _ = False

  getVars :: TypeExpr -> [String]
  getVars (Equation    l r) = getVars l ++ getVars r
  getVars (Implication l r) = getVars l ++ getVars r
  getVars (UniversalQ  _ e) = getVars e
  getVars (TypeVariable  s) = [s]

  getFreeVars :: TypeExpr -> [String]
  getFreeVars (Equation    l r) = getFreeVars l ++ getFreeVars r
  getFreeVars (Implication l r) = getFreeVars l ++ getFreeVars r
  getFreeVars (UniversalQ  s e) = (getVars e) \\ [s]
  getFreeVars (TypeVariable  s) = [s]

  getMatcher :: [TypeExpr] -> Matcher
  getMatcher list = foldl addExpr Map.empty list
    where
      addExpr :: Matcher -> TypeExpr -> Matcher
      addExpr m (Equation l r) = case l of
        TypeVariable s -> Map.insert s r m
        _              -> m
      addExpr m _ = m

  insertType :: Matcher -> TypeExpr -> TypeExpr
  insertType m (Equation    l r) = Equation (insertType m l) (insertType m r)
  insertType m (Implication l r) = Implication (insertType m l) (insertType m r)
  insertType m (UniversalQ  s e) = UniversalQ s (insertType (Map.delete s m) e)
  insertType m (TypeVariable  s) = case Map.lookup s m of
    Just x  -> x
    Nothing -> TypeVariable s

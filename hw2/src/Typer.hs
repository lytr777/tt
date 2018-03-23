module Typer
  ( getTypeAnnotation
  , TypeExpr (..)
  , Context
  ) where

    import Control.Monad.State
    import Simplifier (Expression (..))
    import qualified Data.Map.Strict as Map

    data TypeExpr = Implication  TypeExpr TypeExpr
                  | Equation     TypeExpr TypeExpr
                  | TypeVariable String

    instance Show TypeExpr where
      show (Equation    l r) = show l ++ " = " ++ show r
      show (Implication l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"
      show (TypeVariable  s) = s

    instance Eq TypeExpr where
      (==) (Implication l1 r1) (Implication l2 r2) = (l1 == l2) && (r1 == r2)
      (==) (Equation    l1 r1) (Equation    l2 r2) = (l1 == l2) && (r1 == r2)
      (==) (TypeVariable   s1) (TypeVariable   s2) = (s1 == s2)
      (==) _                   _                   = False

    type Context = Map.Map String TypeExpr
    type TypeState = State Integer ([TypeExpr], Context, TypeExpr)

    getTypeAnnotation :: Expression -> ([TypeExpr], Context, TypeExpr)
    getTypeAnnotation e = fst $ runState (getTypeState Map.empty e) 0

    getTypeState :: Context -> Expression -> TypeState
    getTypeState c (Application l r) = do
      leftResult  <- getTypeState c l
      rightResult <- getTypeState c r

      index <- get
      modify (+1)
      let var = TypeVariable $ getTypeVar index

      let lrList = (_1 leftResult) ++ (_1 rightResult)
      let newCtx = Map.union (_2 leftResult) (_2 rightResult)
      let newEq = Equation (_3 leftResult) (Implication (_3 rightResult) var)
      return (lrList ++ [newEq], newCtx , var)
    getTypeState c (Lambda s e) = do
      index <- get
      modify (+1)
      let var = TypeVariable $ getTypeVar index

      result <- getTypeState (Map.insert s var c) e
      return (_1 result, _2 result, Implication var (_3 result))
    getTypeState c (Variable s) = do
      case (Map.lookup s c) of
        Just t  -> return ([], Map.empty, t)
        Nothing -> do
          index <- get
          modify (+1)
          let var = TypeVariable $ getTypeVar index
          return ([], Map.fromList [(s, var)], var)

    getTypeVar :: Integer -> String
    getTypeVar i = "a" ++ show i

    _1 :: (a, b, c) -> a
    _1 (x, _, _) = x

    _2 :: (a, b, c) -> b
    _2 (_, x, _) = x

    _3 :: (a, b, c) -> c
    _3 (_, _, x) = x

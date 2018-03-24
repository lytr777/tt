module Simplifier
  ( reformatExpr
  , Expression (..)
  ) where

    import Struct

    data Expression = Application Expression Expression
                    | Lambda      String Expression
                    | Variable    String

    instance Show Expression where
      show (Application l r) = "(" ++ show l ++ " " ++ show r ++ ")"
      show (Lambda      s e) = "(\\" ++ s ++ ". " ++ show e ++ ")"
      show (Variable    s  ) = s

    instance Eq Expression where
      (==) (Application l1 r1) (Application l2 r2) = (l1 == l2) && (r1 == r2)
      (==) (Lambda      s1 e1) (Lambda      s2 e2) = (s1 == s2) && (e1 == e2)
      (==) (Variable    s1   ) (Variable    s2   ) = (s1 == s2)
      (==) _                   _                   = False

    instance Ord Expression where
      compare (Application l1 r1) (Application l2 r2) = getOrdering (compare l1 l2) (compare r1 r2)
      compare (Lambda      s1 e1) (Lambda      s2 e2) = getOrdering (compare s1 s2) (compare e1 e2)
      compare (Variable    s1   ) (Variable    s2   ) = compare s1 s2
      compare (Lambda      _  _ ) _                   = GT
      compare _                   (Lambda      _  _ ) = LT
      compare (Application _  _ ) _                   = GT
      compare _                   (Application _  _ ) = LT

    getOrdering :: Ordering -> Ordering -> Ordering
    getOrdering EQ e  = e
    getOrdering LT _  = LT
    getOrdering GT _  = GT

    reformatExpr :: Expr -> Expression
    reformatExpr = uncoverExpr

    uncoverExpr :: Expr -> Expression
    uncoverExpr (EAVE a v e) = Application (uncoverApp a) (Lambda (uncoverVarName v) (uncoverExpr e))
    uncoverExpr (EVE    v e) = Lambda (uncoverVarName v) (uncoverExpr e)
    uncoverExpr (EA   a    ) = uncoverApp a

    uncoverApp :: App -> Expression
    uncoverApp (AAA app a) = Application (uncoverApp app) (uncoverAtom a)
    uncoverApp (AA      a) = uncoverAtom a

    uncoverAtom :: Atom -> Expression
    uncoverAtom (AE e) = uncoverExpr e
    uncoverAtom (AV v) = uncoverVar v

    uncoverVar :: Var -> Expression
    uncoverVar (VS s) = Variable s

    uncoverVarName :: Var -> String
    uncoverVarName (VS s) = s

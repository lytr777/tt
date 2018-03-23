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

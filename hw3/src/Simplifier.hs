module Simplifier
  ( reformatExpr
  , Expression (..)
  ) where

    import Struct

    data Expression = Application Expression Expression
                    | Lambda      String Expression
                    | Let         String Expression Expression
                    | Variable    String

    instance Show Expression where
      show (Application l r) = "(" ++ show l ++ " " ++ show r ++ ")"
      show (Lambda    s e  ) = "(\\" ++ s ++ ". " ++ show e ++ ")"
      show (Let       s e i) = "let " ++ s ++ " = [" ++ show e ++ "] in [" ++ show i ++ "]"
      show (Variable  s    ) = s

    reformatExpr :: Expr -> Expression
    reformatExpr = uncoverExpr

    uncoverExpr :: Expr -> Expression
    uncoverExpr (AVEE v e i) = Let (uncoverVarName v) (uncoverExpr e) (uncoverExpr i)
    uncoverExpr (AAb  ab   ) = uncoverAbs ab

    uncoverAbs :: Abs -> Expression
    uncoverAbs (EAVA a v ab) = Application (uncoverApp a) (Lambda (uncoverVarName v) (uncoverAbs ab))
    uncoverAbs (EVA    v ab) = Lambda (uncoverVarName v) (uncoverAbs ab)
    uncoverAbs (EA   a     ) = uncoverApp a

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

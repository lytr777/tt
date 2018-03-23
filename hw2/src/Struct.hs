module Struct
  ( Expr (..)
  , App (..)
  , Atom (..)
  , Var (..)
  ) where

  data Expr = EAVE App Var Expr
            | EVE Var Expr
            | EA App

  data App = AAA App Atom
           | AA Atom

  data Atom = AE Expr
            | AV Var

  data Var = VS String

  instance Show Var
    where
      show (VS s) = s

  instance Show Atom
    where
      show (AE e) = "(" ++ show e ++ ")"
      show (AV v) = show v

  instance Show App
    where
      show (AAA app atom) = "(" ++ show app ++ " " ++ show atom ++ ")"
      show (AA a)         = show a

  instance Show Expr
    where
      show (EAVE a v e) = show a ++ " \\" ++ show v ++ " . " ++ show e
      show (EVE v e)    = "\\" ++ show v ++ " . " ++ show e
      show (EA a)       = show a

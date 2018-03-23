module Struct
  ( Expr (..)
  , Abs (..)
  , App (..)
  , Atom (..)
  , Var (..)
  ) where

  data Expr = AVEE Var Expr Expr
            | AAb Abs

  data Abs = EAVA App Var Abs
           | EVA Var Abs
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
      show (AAA app a) = "(" ++ show app ++ " " ++ show a ++ ")"
      show (AA      a) = show a

  instance Show Abs
    where
      show (EAVA a v ab) = show a ++ " \\" ++ show v ++ " . " ++ show ab
      show (EVA    v ab) = "\\" ++ show v ++ " . " ++ show ab
      show (EA   a     ) = show a

  instance Show Expr
    where
      show (AVEE v e i) = "let " ++ show v ++ " = " ++ show e ++ " in " ++ show i
      show (AAb  ab   ) = show ab

{-# LANGUAGE DeriveFoldable #-}

module AST
  ( Decl (..)
  , Expr (..)
  , Name (..)
  , Op (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

data Decl a
  = Decl a (Name a) (Expr a)
  deriving (Foldable, Show)

data Expr a
  = Einteger a Integer
  | Edouble a Double
  | Evar a (Name a)
  | Epar a (Expr a)
  | Eneg a (Expr a)
  | Eop a (Expr a) (Op a) (Expr a)
  deriving (Foldable, Show)

data Name a
  = Name a ByteString
  deriving (Foldable, Show)

data Op a
  = Oplus a
  | Ominus a
  | Otimes a
  | Odivide a
  deriving (Foldable, Show)

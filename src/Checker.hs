module Checker
  ( check
  , Checked
  ) where

import AST
  ( Decl (..)
  , Expr (..)
  , Name (..)
  )

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashSet as HashSet
import qualified Lexer as L

data Checked
  = Checked
  deriving (Eq, Show)

check :: [Decl L.Range] -> Either String Checked
check = check' HashSet.empty
 where
  check' :: HashSet.HashSet ByteString -> [Decl L.Range] -> Either String Checked
  check' _ [] = Right Checked
  check' table ((Decl _ (Name _ identifier) declExpr) : decls)
    | declResult == Right Checked = check' (HashSet.insert identifier table) decls
    | otherwise = declResult
   where
    declResult = checkExpr table declExpr
     where
      checkExpr :: HashSet.HashSet ByteString -> Expr L.Range -> Either String Checked
      checkExpr _ (Einteger _ _) = Right Checked
      checkExpr _ (Edouble _ _) = Right Checked
      checkExpr table' (Evar (L.Range (L.AlexPn _ ln col) _) (Name _ varIdentifier))
        | HashSet.member varIdentifier table' = Right Checked
        | otherwise =
            Left $
              "error: undeclared variable "
                ++ BS.unpack varIdentifier
                ++ " at line "
                ++ show ln
                ++ ", column "
                ++ show col
      checkExpr table' (Epar _ expr) = checkExpr table' expr
      checkExpr table' (Eneg _ expr) = checkExpr table' expr
      checkExpr table' (Eop _ expr _ expr')
        | exprResult == Right Checked = checkExpr table' expr'
        | otherwise = exprResult
       where
        exprResult = checkExpr table' expr

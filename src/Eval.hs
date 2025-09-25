module Eval
  ( eval
  , Vars
  ) where

import Control.Monad (foldM)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Range
  ( AlexPosn (..)
  , Range (..)
  )

import AST
  ( Decl (..)
  , Expr (..)
  , Name (..)
  , Op (..)
  )

type Vars = HM.HashMap ByteString Number
type RangedDecl = Decl Range

data Number
  = Ninteger Integer
  | Ndouble Double
  deriving (Show)

instance Num Number where
  (+) (Ninteger a) (Ninteger b) = Ninteger $ a + b
  (+) (Ndouble a) (Ndouble b) = Ndouble $ a + b
  (+) (Ninteger a) (Ndouble b) = Ndouble $ fromIntegral a + b
  (+) (Ndouble a) (Ninteger b) = Ndouble $ a + fromIntegral b

  (*) (Ninteger a) (Ninteger b) = Ninteger $ a * b
  (*) (Ndouble a) (Ndouble b) = Ndouble $ a * b
  (*) (Ninteger a) (Ndouble b) = Ndouble $ fromIntegral a * b
  (*) (Ndouble a) (Ninteger b) = Ndouble $ a * fromIntegral b

  abs (Ninteger a) = Ninteger $ abs a
  abs (Ndouble a) = Ndouble $ abs a

  signum (Ninteger a) = Ninteger $ signum a
  signum (Ndouble a) = Ndouble $ signum a

  fromInteger = Ninteger

  negate (Ninteger a) = Ninteger $ negate a
  negate (Ndouble a) = Ndouble $ negate a

instance Fractional Number where
  fromRational = Ndouble . fromRational

  (/) (Ninteger a) (Ninteger b) = Ndouble $ fromIntegral a / fromIntegral b
  (/) (Ninteger a) (Ndouble b) = Ndouble $ fromIntegral a / b
  (/) (Ndouble a) (Ninteger b) = Ndouble $ a / fromIntegral b
  (/) (Ndouble a) (Ndouble b) = Ndouble $ a / b

evalExpr :: Vars -> Expr Range -> Either String Number
evalExpr _ (Einteger _ x) = Right $ Ninteger x
evalExpr _ (Edouble _ x) = Right $ Ndouble x
evalExpr vars (Evar (Range (AlexPn _ ln col) _) (Name _ name)) =
  maybe
    ( Left $
        "error: undeclared variable "
          ++ BS.unpack name
          ++ " at line "
          ++ show ln
          ++ ", column "
          ++ show col
    )
    Right
    $ HM.lookup name vars
evalExpr vars (Epar _ expr) = evalExpr vars expr
evalExpr vars (Eneg _ expr) =
  negate <$> evalExpr vars expr
evalExpr vars (Eop _ expr' op expr'') = do
  v' <- evalExpr vars expr'
  v'' <- evalExpr vars expr''
  case op of
    Oplus _ -> Right $ v' + v''
    Ominus _ -> Right $ v' - v''
    Otimes _ -> Right $ v' * v''
    Odivide (Range (AlexPn _ ln col) _) -> divide v' v'' ln col
 where
  divide :: Number -> Number -> Int -> Int -> Either String Number
  divide v' v'' ln col
    | isZero v'' = Left $ divideErrorMessage ln col
    | otherwise = Right $ v' / v''

  divideErrorMessage :: Int -> Int -> String
  divideErrorMessage ln col = "error: division by 0 at line " ++ show ln ++ ", column " ++ show col

  isZero :: Number -> Bool
  isZero (Ninteger 0) = True
  isZero (Ndouble 0) = True
  isZero _ = False

evalDecl :: Vars -> RangedDecl -> Either String Vars
evalDecl vars (Decl _ (Name _ name) expr) = do
  evaluatedExpr <- evalExpr vars expr
  return $ HM.insert name evaluatedExpr vars

eval :: [RangedDecl] -> Either String Vars
eval = foldM evalDecl HM.empty

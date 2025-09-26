module EvalSpec
  ( spec
  ) where

import AST (Decl)
import Data.ByteString.Lazy.Char8
  ( ByteString
  , unpack
  )
import qualified Data.HashMap.Strict as HM
import Eval
  ( Number (..)
  , Vars
  , eval
  )
import ParserSpec (rangedDeclsFromInput)
import Range (Range (..))
import Test.Hspec

parse :: ByteString -> Either String [Decl Range]
parse = rangedDeclsFromInput

vars :: ByteString -> Either String Vars
vars input = do
  ast <- parse input
  eval ast

divideError :: Int -> Int -> Either String Vars
divideError ln col =
  Left $ "error: division by 0 at line " ++ show ln ++ ", column " ++ show col

undeclaredVariableError :: ByteString -> Int -> Int -> Either String Vars
undeclaredVariableError var ln col =
  Left $
    "error: undeclared variable "
      ++ unpack var
      ++ " at line "
      ++ show ln
      ++ ", column "
      ++ show col

spec :: Spec
spec = do
  describe "integer arithmetic" $ do
    it "can do just addition and subtraction" $ do
      vars "a = 3 + (5 - 4 * 8); b = 20 * (1 - 3); a = a + 2 * b;"
        `shouldBe` Right (HM.fromList [("a", Ninteger (-104)), ("b", Ninteger (-40))])

    it "can also do multiplication" $ do
      vars "a = 3 * (5 - 6); a = (a + 7) * (a - 10);"
        `shouldBe` Right (HM.fromList [("a", Ninteger (-52))])

  describe "float arithmetic" $ do
    it "can do addition, subtraction, multiplication and division" $ do
      vars "a = 3.5 - 4.17 * 0.99; b = a / 1.25; a = (b + a) / 6;"
        `shouldBe` Right (HM.fromList
                         [("a", Ndouble (-0.18849000000000007)), ("b", Ndouble (-0.5026400000000002))])

    it "prohibits division by zero" $ do
      vars "a = 3.5 + 7; a = a / (a - a);" `shouldBe` divideError 1 20

    it "integer division leads to float" $ do
      vars "a = 6 / 2; b = a / 1;"
        `shouldBe` Right (HM.fromList [("a", Ndouble 3.0), ("b", Ndouble 3.0)])

  describe "error-handling" $ do
    it "prohibits use of undeclared variables" $ do
      vars "a = b + c; b = a - c; c = a * b;"
        `shouldBe` undeclaredVariableError "b" 1 5

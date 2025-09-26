module InterpreterSpec
  ( spec
  ) where

import Data.HashMap.Strict (fromList)
import Eval (Number (..))
import Interpreter (interpret)
import Test.Hspec

--     "error: undeclared variable "
--       ++ unpack var
--       ++ " at line "
--       ++ show ln
--       ++ ", column "
--       ++ show col

spec :: Spec
spec = do
  describe "success interpretation" $ do
    it "parses simple assignments chain" $ do
      interpret "a = 3; b = 5; c = 7; a = a * (c - b) + c / b;"
        `shouldBe` Right (fromList [("a", Ndouble 7.4), ("b", Ninteger 5), ("c", Ninteger 7)])
    it "parses more complex assignments" $ do
      interpret
        "apple = 3.5 * (3.4 - 1.3); banana = apple * 0.136 / 4 * 100; carrot = 100 - banana; carrot = carrot - 100 * (1 / 10000);\napple = apple + (carrot - banana);"
        `shouldBe` Right
          ( fromList
              [ ("apple", Ndouble 57.36000000000001)
              , ("banana", Ndouble 24.99)
              , ("carrot", Ndouble 75.0)
              ]
          )
  describe "error-handling" $ do
    it "may return lexical error" $ do
      interpret "Apple = 3.5 * (3.4 - 1.3);"
        `shouldBe` Left "lexical error at line 1, column 1"

    it "may return syntax error" $ do
      interpret "chair = table (and + bed);"
        `shouldBe` Left "syntax error at line 1, column 15"

    it "may return division-by-zero error" $ do
      interpret "a = 3.5; b = a / (a - 3.5);"
        `shouldBe` Left "error: division by 0 at line 1, column 16"

    it "may return undeclared-variable error" $ do
      interpret "bike = car - 3.14;"
        `shouldBe` Left "error: undeclared variable car at line 1, column 8"

module ParserSpec
  ( rangedDeclsFromInput
  , spec
  ) where

import qualified AST
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Lexer as L
import Parser (parse)
import Range (Range)
import Test.Hspec

data Decl
  = Decl Name Expr
  deriving (Eq, Show)

data Expr
  = Einteger Integer
  | Edouble Double
  | Evar Name
  | Epar Expr
  | Eneg Expr
  | Eop Expr Op Expr
  deriving (Eq, Show)

data Name
  = Name ByteString
  deriving (Eq, Show)

data Op
  = Oplus
  | Ominus
  | Otimes
  | Odivide
  deriving (Eq, Show)

declFromRanged :: AST.Decl a -> Decl
declFromRanged (AST.Decl _ name expr) = Decl (nameFromRanged name) (exprFromRanged expr)

exprFromRanged :: AST.Expr a -> Expr
exprFromRanged (AST.Einteger _ integer) = Einteger integer
exprFromRanged (AST.Edouble _ double) = Edouble double
exprFromRanged (AST.Evar _ name) = Evar $ nameFromRanged name
exprFromRanged (AST.Epar _ expr) = Epar $ exprFromRanged expr
exprFromRanged (AST.Eneg _ expr) = Eneg $ exprFromRanged expr
exprFromRanged (AST.Eop _ expr' op expr'') = Eop (exprFromRanged expr') (opFromRanged op) (exprFromRanged expr'')

nameFromRanged :: AST.Name a -> Name
nameFromRanged (AST.Name _ name) = Name name

opFromRanged :: AST.Op a -> Op
opFromRanged (AST.Oplus _) = Oplus
opFromRanged (AST.Ominus _) = Ominus
opFromRanged (AST.Otimes _) = Otimes
opFromRanged (AST.Odivide _) = Odivide

rangedDeclsFromInput :: ByteString -> Either String [AST.Decl Range]
rangedDeclsFromInput input = L.runAlex input parse

declsFromRanged :: [AST.Decl Range] -> [Decl]
declsFromRanged = map declFromRanged

declsFromInput :: ByteString -> Either String [Decl]
declsFromInput input = do
  rangedDecls <- rangedDeclsFromInput input
  return $ declsFromRanged rangedDecls

declFromInput :: ByteString -> Either String Decl
declFromInput input = do
  decls <- declsFromInput input
  case decls of
    (d : _) -> Right d
    _ -> noDecls
     where
      noDecls :: Either String Decl
      noDecls = Left "no declarations found"

namesFromDeclsFromInput :: ByteString -> Either String [Name]
namesFromDeclsFromInput input = do
  decls <- declsFromInput input
  return $ fmap (\(Decl name _) -> name) decls

nameFromDeclFromInput :: ByteString -> Either String Name
nameFromDeclFromInput input = do
  Decl name _ <- declFromInput input
  return name

exprFromDeclFromInput :: ByteString -> Either String Expr
exprFromDeclFromInput input = do
  Decl _ expr <- declFromInput input
  return expr

opFromExprFromDeclFromInput :: ByteString -> Either String Op
opFromExprFromDeclFromInput input = do
  expr <- exprFromDeclFromInput input
  case expr of
    Eop _ op _ -> Right op
    _ -> notOpExpr
     where
      notOpExpr :: Either String Op
      notOpExpr = Left "not operation expression"

syntaxError :: Int -> Int -> Either String [Decl]
syntaxError ln col = Left $ syntaxErrorMessage ln col
 where
  syntaxErrorMessage :: Int -> Int -> String
  syntaxErrorMessage ln' col' = "syntax error at line " ++ show ln' ++ ", column " ++ show col'

defaultSyntaxError :: Either String [Decl]
defaultSyntaxError = syntaxError 1 1

spec :: Spec
spec = do
  describe "names" $ do
    it "allows using variables in declarations" $ do
      nameFromDeclFromInput "myVar = a + 5;" `shouldBe` Right (Name "myVar")

    it "can parse multiple declarations" $ do
      namesFromDeclsFromInput "a = 5; b = a - 3; c = x * (a + b);"
        `shouldBe` Right [Name "a", Name "b", Name "c"]

    it "prohibits declarations not starting with name" $ do
      declsFromInput "1 + 2 = a;" `shouldBe` defaultSyntaxError

  describe "expressions" $ do
    it "considers integers as expressions" $ do
      exprFromDeclFromInput "a = 1;" `shouldBe` Right (Einteger 1)

    it "considers floats as expressions" $ do
      exprFromDeclFromInput "b = 4.5;" `shouldBe` Right (Edouble 4.5)

    it "considers names in expressions as expressions themselves" $ do
      exprFromDeclFromInput "x = y + 1;"
        `shouldBe` Right (Eop (Evar $ Name "y") Oplus (Einteger 1))

    it "considers expressions in parentheses as expressions themselves" $ do
      exprFromDeclFromInput "tuple = (type + comma);"
        `shouldBe` Right (Epar $ Eop (Evar $ Name "type") Oplus (Evar $ Name "comma"))

    it "considers expressions' negations as expressions themselves" $ do
      exprFromDeclFromInput "abs = -abs;"
        `shouldBe` Right (Eneg $ Evar $ Name "abs")

    it "can parse some complex expressions" $ do
      exprFromDeclFromInput "len = (2 - y * 0) * pi * r;"
        `shouldBe` Right
          ( Eop
              ( Eop
                  (Epar (Eop (Einteger 2) Ominus (Eop (Evar $ Name "y") Otimes (Einteger 0))))
                  Otimes
                  (Evar $ Name "pi")
              )
              Otimes
              (Evar $ Name "r")
          )

  describe "operators" $ do
    it "allows <expr> + <expr>" $ do
      opFromExprFromDeclFromInput "h2o = h2 + o2;" `shouldBe` Right Oplus

    it "allows <expr> - <expr>" $ do
      opFromExprFromDeclFromInput "e = pi - 0.25;" `shouldBe` Right Ominus

    it "allows <expr> * <expr>" $ do
      opFromExprFromDeclFromInput "square = pi * r * r;" `shouldBe` Right Otimes

    it "allows <expr> / <expr>" $ do
      opFromExprFromDeclFromInput "speed = distance / time;" `shouldBe` Right Odivide

  describe "declarations" $ do
    it "allows empty input" $ do
      declsFromInput "\t\n \t" `shouldBe` Right []

    it "allows declarations chains" $ do
      declsFromInput "a = 3; b = a * 5; a = a + b;"
        `shouldBe` Right
          [ Decl (Name "a") (Einteger 3)
          , Decl (Name "b") (Eop (Evar $ Name "a") Otimes (Einteger 5))
          , Decl (Name "a") (Eop (Evar $ Name "a") Oplus (Evar $ Name "b"))
          ]

  describe "error-handling" $ do
    it "allows only declarations" $ do
      declsFromInput "a = 7 - 8.5; b = a * 0;\na - b;" `shouldBe` syntaxError 2 3

    it "considers only '=' as assignment operator" $ do
      declsFromInput "a += a;" `shouldBe` syntaxError 1 3

    it "allows only correct parentheses' sequences" $ do
      declsFromInput "x = (y + z) - a);" `shouldBe` syntaxError 1 16

    it "does not consider <empty space>; as empty declaration" $ do
      declsFromInput "    ;  ;\n;" `shouldBe` syntaxError 1 5

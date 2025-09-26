module LexerSpec
  ( spec
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Lexer
import Test.Hspec

rangedTokensFromInput :: ByteString -> Either String [RangedToken]
rangedTokensFromInput input = runAlex input tokenize

tokenFromRanged :: RangedToken -> Token
tokenFromRanged = rtToken

tokensFromRanged :: [RangedToken] -> [Token]
tokensFromRanged = map tokenFromRanged

tokensFromInput :: ByteString -> Either String [Token]
tokensFromInput input = do
  rangedTokens <- rangedTokensFromInput input
  return $ tokensFromRanged rangedTokens

tokensWithoutEOF :: ByteString -> Either String [Token]
tokensWithoutEOF input = do
  tokens <- tokensFromInput input
  return $ init tokens

lexerError :: Int -> Int -> Either String [Token]
lexerError ln col = Left $ lexerErrorMessage ln col
 where
  lexerErrorMessage :: Int -> Int -> String
  lexerErrorMessage ln' col' = "lexical error at line " ++ show ln' ++ ", column " ++ show col'

defaultLexerError :: Either String [Token]
defaultLexerError = lexerError 1 1

spec :: Spec
spec = do
  describe "identifiers" $ do
    it "allows single-letter names" $ do
      tokensWithoutEOF "a" `shouldBe` Right [Tvarid "a"]

    it "allows multi-letter names" $ do
      tokensWithoutEOF "hello" `shouldBe` Right [Tvarid "hello"]

    it "allows letters of both cases" $ do
      tokensWithoutEOF "myVar" `shouldBe` Right [Tvarid "myVar"]

    it "prohibits letters with first capital letters" $ do
      tokensWithoutEOF "NotMyVar" `shouldBe` defaultLexerError

    it "allows non-starting digits" $ do
      tokensWithoutEOF "app1e2banana" `shouldBe` Right [Tvarid "app1e2banana"]

    it "considers <integer><name> as distinct tokens" $ do
      tokensWithoutEOF "0range" `shouldBe` Right [Tinteger 0, Tvarid "range"]

    it "allows non-starting underscores and apostrophes" $ do
      tokensWithoutEOF "rock'n'roll_" `shouldBe` Right [Tvarid "rock'n'roll_"]

    it "prohibits starting underscores" $ do
      tokensWithoutEOF "_flat" `shouldBe` defaultLexerError

    it "prohibits starting apostrophes" $ do
      tokensWithoutEOF "'coffee" `shouldBe` defaultLexerError

    it "allows being identifiers and symbols too close" $ do
      tokensWithoutEOF ")smile(sad+mood)=smileSadMood;"
        `shouldBe` Right
          [ Tcparen
          , Tvarid "smile"
          , Toparen
          , Tvarid "sad"
          , Tplus
          , Tvarid "mood"
          , Tcparen
          , Tequal
          , Tvarid "smileSadMood"
          , Tsemicolon
          ]

    it "can tokenize whole English sentence" $ do
      tokensWithoutEOF "snake_case is prettier than camelCase =)"
        `shouldBe` Right
          [ Tvarid "snake_case"
          , Tvarid "is"
          , Tvarid "prettier"
          , Tvarid "than"
          , Tvarid "camelCase"
          , Tequal
          , Tcparen
          ]

  describe "integers" $ do
    it "considers <digit_sequence> as integer" $ do
      tokensWithoutEOF "10" `shouldBe` Right [Tinteger 10]

    it "allows too many leading zeros" $ do
      tokensWithoutEOF "000123" `shouldBe` Right [Tinteger 123]

    it "allows values bigger than 2^64 - 1" $ do
      tokensWithoutEOF "92233720368547758070000"
        `shouldBe` Right [Tinteger 92233720368547758070000]

    it "can tokenize some arithmetic expressions" $ do
      tokensWithoutEOF "10 * (9 / 10) = 9"
        `shouldBe` Right
          [ Tinteger 10
          , Ttimes
          , Toparen
          , Tinteger 9
          , Tdivide
          , Tinteger 10
          , Tcparen
          , Tequal
          , Tinteger 9
          ]

  describe "floats" $ do
    it "recognizes float values as well as integers" $ do
      tokensWithoutEOF "4.5692" `shouldBe` Right [Tdouble 4.5692]

    it "allows leading zeros here too" $ do
      tokensWithoutEOF "000707.562" `shouldBe` Right [Tdouble 707.562]

    it "prohibits .<float_sequence> notation" $ do
      tokensWithoutEOF ".14" `shouldBe` defaultLexerError

    it "prohibits <float_sequence> notation" $ do
      tokensWithoutEOF "0." `shouldBe` lexerError 1 2

    it "can tokenize some more complex mathematical stuff" $ do
      tokensWithoutEOF "((5.0 - 4.0) * sin(x) * sin(x) + cos(x) * cos(x) = 1.0)"
        `shouldBe` Right
          [ Toparen
          , Toparen
          , Tdouble 5.0
          , Tminus
          , Tdouble 4.0
          , Tcparen
          , Ttimes
          , Tvarid "sin"
          , Toparen
          , Tvarid "x"
          , Tcparen
          , Ttimes
          , Tvarid "sin"
          , Toparen
          , Tvarid "x"
          , Tcparen
          , Tplus
          , Tvarid "cos"
          , Toparen
          , Tvarid "x"
          , Tcparen
          , Ttimes
          , Tvarid "cos"
          , Toparen
          , Tvarid "x"
          , Tcparen
          , Tequal
          , Tdouble 1.0
          , Tcparen
          ]

    it "still considers <float><name> as two distinct tokens" $ do
      tokensWithoutEOF "3.14e*pi"
        `shouldBe` Right [Tdouble 3.14, Tvarid "e", Ttimes, Tvarid "pi"]

  describe "symbols" $ do
    it "allows plus, minus, times and divide operators" $ do
      tokensWithoutEOF "++/ lostVar -*+"
        `shouldBe` Right [Tplus, Tplus, Tdivide, Tvarid "lostVar", Tminus, Ttimes, Tplus]

    it "allows opening and closing parentheses" $ do
      tokensWithoutEOF "((()()))("
        `shouldBe` Right
          [ Toparen
          , Toparen
          , Toparen
          , Tcparen
          , Toparen
          , Tcparen
          , Tcparen
          , Tcparen
          , Toparen
          ]

    it "considers '=' as assignment operator" $ do
      tokensWithoutEOF "a = 42 * 2.3;"
        `shouldBe` Right [Tvarid "a", Tequal, Tinteger 42, Ttimes, Tdouble 2.3, Tsemicolon]

    it "considers ';' as assignment ending" $ do
      tokensWithoutEOF
        "pi = 3.14; notice_consecutive_semicolons_are_not_allowed_beyond_lexer;;"
        `shouldBe` Right
          [ Tvarid "pi"
          , Tequal
          , Tdouble 3.14
          , Tsemicolon
          , Tvarid "notice_consecutive_semicolons_are_not_allowed_beyond_lexer"
          , Tsemicolon
          , Tsemicolon
          ]

  describe "error-handling" $ do
    it "prohibits undefined symbols" $ do
      tokensWithoutEOF "you & me" `shouldBe` lexerError 1 5

    it "stops at the first misspelling" $ do
      tokensWithoutEOF "12 | 34 ^ 56 $ 78" `shouldBe` lexerError 1 4

    it "traces not only columns, but also lines" $ do
      tokensWithoutEOF "a = 5;\nb = a ^ a;" `shouldBe` lexerError 2 7

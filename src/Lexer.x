{
module Lexer
  ( Alex
  , alexError
  , alexGetInput
  , alexMonadScan
  , AlexPosn (..)
  , Range (..)
  , RangedToken (..)
  , runAlex
  , Token (..)
  , tokenize'
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Token
  ( Token(..)
  )
}

%wrapper "monad-bytestring"

$digit = [0-9]
$small = [a-z]
$large = [A-Z]
$idchar = [$small $large $digit \_ \']

@varid = $small $idchar*
@integer = $digit+
@double = $digit+ \. $digit+
@oparen = "("
@cparen = ")"
@plus = "+"
@minus = "-"
@times = "*"
@divide = "/"
@equal = "="
@semicolon = ";"

tokens :-

$white+ ;

<0> @varid { tokVarid }
<0> @integer { tokInteger }
<0> @double { tokDouble }

<0> @oparen { tokOparen }
<0> @cparen { tokCparen }
<0> @plus { tokPlus }
<0> @minus { tokMinus }
<0> @times { tokTimes }
<0> @divide { tokDivide }
<0> @equal { tokEqual }
<0> @semicolon { tokSemicolon }

{
data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  }
  deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  }
  deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start', _, str, _) len = Range{start = start', stop = stop'}
 where
  stop' = BS.foldl' alexMove start' $ BS.take len str

alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken Teof (Range pos pos)

tokVarid :: AlexAction RangedToken
tokVarid input@(_, _, str, _) len =
  pure
    RangedToken
      { rtToken = Tvarid $ BS.take len str
      , rtRange = mkRange input len
      }

tokInteger :: AlexAction RangedToken
tokInteger input@(_, _, str, _) len =
  pure
    RangedToken
      { rtToken = Tinteger $ read $ BS.unpack $ BS.take len str
      , rtRange = mkRange input len
      }

tokDouble :: AlexAction RangedToken
tokDouble input@(_, _, str, _) len =
  pure
    RangedToken
      { rtToken = Tdouble $ read $ BS.unpack $ BS.take len str
      , rtRange = mkRange input len
      }

tokOp :: Token -> AlexAction RangedToken
tokOp ctor input len =
  pure
    RangedToken
      { rtToken = ctor
      , rtRange = mkRange input len
      }

tokPlus :: AlexAction RangedToken
tokPlus = tokOp Tplus

tokMinus :: AlexAction RangedToken
tokMinus = tokOp Tminus

tokTimes :: AlexAction RangedToken
tokTimes = tokOp Ttimes

tokDivide :: AlexAction RangedToken
tokDivide = tokOp Tdivide

tokEqual :: AlexAction RangedToken
tokEqual = tokOp Tequal

tokOparen :: AlexAction RangedToken
tokOparen = tokOp Toparen

tokCparen :: AlexAction RangedToken
tokCparen = tokOp Tcparen

tokSemicolon :: AlexAction RangedToken
tokSemicolon input len =
  pure
    RangedToken
      { rtToken = Tsemicolon
      , rtRange = mkRange input len
      }

tokenize :: Alex [RangedToken]
tokenize = do
    gotToken <- alexMonadScan
    if rtToken gotToken == Teof
      then pure [gotToken]
      else (gotToken :) <$> tokenize

tokenize' :: ByteString -> Either String [RangedToken]
tokenize' input = runAlex input tokenize
}

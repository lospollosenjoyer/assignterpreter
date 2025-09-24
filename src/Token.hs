module Token
  ( Token (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

data Token
  = Tvarid ByteString
  | Tinteger Integer
  | Tfractional Double
  | Tplus
  | Tminus
  | Ttimes
  | Tdivide
  | Tequal
  | Toparen
  | Tcparen
  | Teof
  deriving (Eq, Show)

module Token
  ( Token (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

data Token
  = Tvarid ByteString
  | Tinteger Integer
  | Tdouble Double
  | Tplus
  | Tminus
  | Ttimes
  | Tdivide
  | Tequal
  | Toparen
  | Tcparen
  | Tsemicolon
  | Teof
  deriving (Eq, Show)

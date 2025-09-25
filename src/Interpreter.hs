module Interpreter
  ( interpret
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Eval
  ( Vars
  , eval
  )
import Lexer
  ( runAlex
  )
import Parser
  ( parse
  )

interpret :: ByteString -> Either String Vars
interpret input = do
  ast <- runAlex input parse
  vars <- eval ast
  return vars

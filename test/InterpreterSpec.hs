module InterpreterSpec
  ( spec
  ) where

import Data.ByteString.Lazy.Char8
  ( ByteString
  , unpack
  )
import Eval
  ( Number (..)
  , Vars
  )
import Interpreter (interpret)
import Test.Hspec

spec :: Spec
spec = undefined

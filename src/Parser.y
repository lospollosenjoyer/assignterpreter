{
module Parser
  ( parse
  ) where

import Data.Maybe (fromJust)
import Data.Monoid (First(..))

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Lexer as L
import AST
  ( Decl(..)
  , Expr(..)
  , Name(..)
  , Op(..)
  )
}

%name parser decls
%tokentype { L.RangedToken }
%error { parserError }

%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.Teof _ }
%expect 0

%token
  VARID       { L.RangedToken (L.Tvarid      _) _ }
  INTEGER     { L.RangedToken (L.Tinteger    _) _ }
  FRACTIONAL  { L.RangedToken (L.Tfractional _) _ }
  '+'         { L.RangedToken L.Tplus           _ }
  '-'         { L.RangedToken L.Tminus          _ }
  '*'         { L.RangedToken L.Ttimes          _ }
  '/'         { L.RangedToken L.Tdivide         _ }
  '='         { L.RangedToken L.Tequal          _ }
  '('         { L.RangedToken L.Toparen         _ }
  ')'         { L.RangedToken L.Tcparen         _ }

%left '+' '-'
%left '*' '/'

%%

many_reversed(p)
  :                     { [] }
  | many_reversed(p) p
                        { $2 : $1 }

many(p)
  : many_reversed(p)
                        { reverse $1 }

name :: { Name L.Range }
  : VARID
                { termFromToken $1 (\range (L.Tvarid name) -> Name range name) }

expr :: { Expr L.Range }
  : INTEGER
                  { termFromToken $1 (\range (L.Tinteger int) -> Einteger range int) }
  | FRACTIONAL
                  { termFromToken $1 (\range (L.Tfractional frac) -> Efractional range frac) }
  | name
                  { Evar (info $1) $1 }
  | '(' expr ')'
                  { Epar (L.rtRange $1 <-> L.rtRange $3) $2 }
  | '-' expr
                  { Eneg (L.rtRange $1 <-> info $2) $2 }
  | expr '+' expr
                  { Eop (info $1 <-> info $3) $1 (Oplus (L.rtRange $2)) $3 }
  | expr '-' expr
                  { Eop (info $1 <-> info $3) $1 (Ominus (L.rtRange $2)) $3 }
  | expr '*' expr
                  { Eop (info $1 <-> info $3) $1 (Otimes (L.rtRange $2)) $3 }
  | expr '/' expr
                  { Eop (info $1 <-> info $3) $1 (Odivide (L.rtRange $2)) $3 }

decl :: { Decl L.Range }
  : name '=' expr
                { Decl (info $1 <-> info $3) $1 $3 }

decls :: { [Decl L.Range] }
  : many(decl)
                { $1 }

{
termFromToken :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
termFromToken (L.RangedToken token range) ctor = ctor range token

info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

(<->) :: L.Range -> L.Range -> L.Range
L.Range aStart _ <-> L.Range _ bStop = L.Range aStart bStop

parserError :: L.RangedToken -> L.Alex a
parserError _ = do
  (L.AlexPn _ ln col, _, _, _) <- L.alexGetInput
  L.alexError $ "syntax error at line " <> show ln <> ", column " <> show col

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

parse :: ByteString -> Either String [Decl L.Range]
parse input = L.runAlex input parser
}

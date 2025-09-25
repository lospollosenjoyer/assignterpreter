{
module Parser
  ( parse
  ) where

import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Lexer as L
import Range
  ( AlexPosn (..)
  , Range (..)
  )

import Token
  ( RangedToken (..)
  , Token (..)
  )

import AST
  ( Decl (..)
  , Expr (..)
  , Name (..)
  , Op (..)
  )
}

%name parse decls
%tokentype { RangedToken }
%error { parseError }

%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { RangedToken Teof _ }
%expect 0

%token
  VARID       { RangedToken (Tvarid      _) _ }
  INTEGER     { RangedToken (Tinteger    _) _ }
  DOUBLE      { RangedToken (Tdouble     _) _ }
  '+'         { RangedToken Tplus           _ }
  '-'         { RangedToken Tminus          _ }
  '*'         { RangedToken Ttimes          _ }
  '/'         { RangedToken Tdivide         _ }
  '='         { RangedToken Tequal          _ }
  '('         { RangedToken Toparen         _ }
  ')'         { RangedToken Tcparen         _ }
  ';'         { RangedToken Tsemicolon      _ }

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

name :: { Name Range }
  : VARID
                { termFromToken $1 (\ range (Tvarid name) -> Name range name) }

expr :: { Expr Range }
  : INTEGER
                  { termFromToken $1 (\ range (Tinteger int) -> Einteger range int) }
  | DOUBLE 
                  { termFromToken $1 (\ range (Tdouble frac) -> Edouble range frac) }
  | name
                  { Evar (info $1) $1 }
  | '(' expr ')'
                  { Epar (rtRange $1 <-> rtRange $3) $2 }
  | '-' expr
                  { Eneg (rtRange $1 <-> info $2) $2 }
  | expr '+' expr
                  { Eop (info $1 <-> info $3) $1 (Oplus (rtRange $2)) $3 }
  | expr '-' expr
                  { Eop (info $1 <-> info $3) $1 (Ominus (rtRange $2)) $3 }
  | expr '*' expr
                  { Eop (info $1 <-> info $3) $1 (Otimes (rtRange $2)) $3 }
  | expr '/' expr
                  { Eop (info $1 <-> info $3) $1 (Odivide (rtRange $2)) $3 }

decl :: { Decl Range }
  : name '=' expr ';'
                      { Decl (info $1 <-> info $3) $1 $3 }

decls :: { [Decl Range] }
  : many(decl)
                { $1 }

{
termFromToken :: RangedToken -> (Range -> Token -> a) -> a
termFromToken (RangedToken token range) ctor = ctor range token

info :: (Foldable f) => f a -> a
info = fromJust . getFirst . foldMap pure

(<->) :: Range -> Range -> Range
Range aStart _ <-> Range _ bStop = Range aStart bStop

parseError :: RangedToken -> L.Alex a
parseError _ = do
  (AlexPn _ ln col, _, _, _) <- L.alexGetInput
  L.alexError $ "syntax error at line " <> show ln <> ", column " <> show col

lexer :: (RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)
}

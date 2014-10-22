{

{-# LANGUAGE OverloadedStrings #-}

-- Happy parser for .scala files. The output is a raw parse tree
-- data structure defined in the "Syntax" module.

module Language.Scala.Parser
    ( Grammar
    , parseWith
    , compilationUnitGrammar
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Either
import           Data.Functor
import           Data.String

import           Language.Scala.Context
import           Language.Scala.Position
import           Language.Scala.Syntax
import           Language.Scala.Tokens
import           Language.Scala.Util

}

%name compilationUnitGrammar compilation_unit
%tokentype { Either Position (Contextual Token) }
%monad { Grammar }
%lexer { lexer } { Left _ }
%error { syntaxError }

%token

  nl_token              { Right ($$ @ (Tok_NewLine One    :@@ _)) }
  nls_token             { Right ($$ @ (Tok_NewLine Many   :@@ _)) }

  "abstract"            { Right ($$ @ (Tok_Abstract       :@@ _)) }
  "case"                { Right ($$ @ (Tok_Case           :@@ _)) }
  "catch"               { Right ($$ @ (Tok_Catch          :@@ _)) }
  "class"               { Right ($$ @ (Tok_Class          :@@ _)) }
  "def"                 { Right ($$ @ (Tok_Def            :@@ _)) }
  "do"                  { Right ($$ @ (Tok_Do             :@@ _)) }
  "else"                { Right ($$ @ (Tok_Else           :@@ _)) }
  "extends"             { Right ($$ @ (Tok_Extends        :@@ _)) }
  "false"               { Right ($$ @ (Tok_False          :@@ _)) }
  "final"               { Right ($$ @ (Tok_Final          :@@ _)) }
  "finally"             { Right ($$ @ (Tok_Finally        :@@ _)) }
  "for"                 { Right ($$ @ (Tok_For            :@@ _)) }
  "forSome"             { Right ($$ @ (Tok_ForSome        :@@ _)) }
  "if"                  { Right ($$ @ (Tok_If             :@@ _)) }
  "implicit"            { Right ($$ @ (Tok_Implicit       :@@ _)) }
  "import"              { Right ($$ @ (Tok_Import         :@@ _)) }
  "lazy"                { Right ($$ @ (Tok_Lazy           :@@ _)) }
  "match"               { Right ($$ @ (Tok_Match          :@@ _)) }
  "new"                 { Right ($$ @ (Tok_New            :@@ _)) }
  "null"                { Right ($$ @ (Tok_Null           :@@ _)) }
  "object"              { Right ($$ @ (Tok_Object         :@@ _)) }
  "override"            { Right ($$ @ (Tok_Override       :@@ _)) }
  "package"             { Right ($$ @ (Tok_Package        :@@ _)) }
  "private"             { Right ($$ @ (Tok_Private        :@@ _)) }
  "protected"           { Right ($$ @ (Tok_Protected      :@@ _)) }
  "return"              { Right ($$ @ (Tok_Return         :@@ _)) }
  "sealed"              { Right ($$ @ (Tok_Sealed         :@@ _)) }
  "super"               { Right ($$ @ (Tok_Super          :@@ _)) }
  "this"                { Right ($$ @ (Tok_This           :@@ _)) }
  "throw"               { Right ($$ @ (Tok_Throw          :@@ _)) }
  "trait"               { Right ($$ @ (Tok_Trait          :@@ _)) }
  "try"                 { Right ($$ @ (Tok_Try            :@@ _)) }
  "true"                { Right ($$ @ (Tok_True           :@@ _)) }
  "type"                { Right ($$ @ (Tok_Type           :@@ _)) }
  "val"                 { Right ($$ @ (Tok_Val            :@@ _)) }
  "var"                 { Right ($$ @ (Tok_Var            :@@ _)) }
  "while"               { Right ($$ @ (Tok_While          :@@ _)) }
  "with"                { Right ($$ @ (Tok_With           :@@ _)) }
  "yield"               { Right ($$ @ (Tok_Yield          :@@ _)) }

  "("                   { Right ($$ @ (Tok_LParen         :@@ _)) }
  ")"                   { Right ($$ @ (Tok_RParen         :@@ _)) }
  "["                   { Right ($$ @ (Tok_LBracket       :@@ _)) }
  "]"                   { Right ($$ @ (Tok_RBracket       :@@ _)) }
  "{"                   { Right ($$ @ (Tok_LBrace         :@@ _)) }
  "}"                   { Right ($$ @ (Tok_RBrace         :@@ _)) }
  "."                   { Right ($$ @ (Tok_Dot            :@@ _)) }
  ","                   { Right ($$ @ (Tok_Comma          :@@ _)) }
  ";"                   { Right ($$ @ (Tok_Semi           :@@ _)) }
  "_"                   { Right ($$ @ (Tok_Underscore     :@@ _)) }
  ":"                   { Right ($$ @ (Tok_Colon          :@@ _)) }
  "="                   { Right ($$ @ (Tok_Equals         :@@ _)) }
  "=>"                  { Right ($$ @ (Tok_Arrow          :@@ _)) }
  "<-"                  { Right ($$ @ (Tok_BackArrow      :@@ _)) }
  "<:"                  { Right ($$ @ (Tok_LowerBound     :@@ _)) }
  "<%"                  { Right ($$ @ (Tok_ViewBound      :@@ _)) }
  ">:"                  { Right ($$ @ (Tok_UpperBound     :@@ _)) }
  "#"                   { Right ($$ @ (Tok_Projection     :@@ _)) }
  "@"                   { Right ($$ @ (Tok_Annotation     :@@ _)) }

  "-"                   { Right ($$ @ (Tok_Op         "-" :@@ _)) }
  "+"                   { Right ($$ @ (Tok_Op         "+" :@@ _)) }
  "~"                   { Right ($$ @ (Tok_Op         "~" :@@ _)) }
  "!"                   { Right ($$ @ (Tok_Op         "!" :@@ _)) }

  op_token              { Right ($$ @ (Tok_Op           _ :@@ _)) }
  varid_token           { Right ($$ @ (Tok_VarId        _ :@@ _)) }
  plainid_token         { Right ($$ @ (Tok_PlainId      _ :@@ _)) }
  stringid_token        { Right ($$ @ (Tok_StringId     _ :@@ _)) }
  
  int_token             { Right ($$ @ (Tok_Int          _ :@@ _)) }
  long_token            { Right ($$ @ (Tok_Long         _ :@@ _)) }
  float_token           { Right ($$ @ (Tok_Float      _ _ :@@ _)) }
  double_token          { Right ($$ @ (Tok_Double     _ _ :@@ _)) }
  char_token            { Right ($$ @ (Tok_Char         _ :@@ _)) }
  string_token          { Right ($$ @ (Tok_String       _ :@@ _)) }
  symbol_token          { Right ($$ @ (Tok_Symbol       _ :@@ _)) }

%%

--
-- literals
--

literal :: { Contextual Literal }:
    signed_int_literal                      { Lit_Int              <\$> $1 }
  | signed_long_literal                     { Lit_Long             <\$> $1 }
  | signed_float_literal                    { (uncurry Lit_Float)  <\$> $1 }
  | signed_double_literal                   { (uncurry Lit_Double) <\$> $1 }
  | boolean_literal                         { Lit_Boolean          <\$> $1 }
  | character_literal                       { Lit_Character        <\$> $1 }
  | string_literal                          { Lit_String           <\$> $1 }
  | symbol_literal                          { Lit_Symbol           <\$> $1 }
  | "null"                                  { Lit_Null :@@ context $1      }

--
-- integer literals
--

signed_int_literal :: { Contextual Integer }:
    int_literal                             { $1 }
  | "-" int_literal                         { context $1 <@@ (negate <\$> $2) }

int_literal :: { Contextual Integer }:
    int_token                               { integerTokenValue <\$> $1 }

signed_long_literal :: { Contextual Integer }:
    long_literal                            { $1 }
  | "-" long_literal                        { context $1 <@@ (negate <\$> $2) }

long_literal :: { Contextual Integer }:
    long_token                              { integerTokenValue <\$> $1 }


--
-- floating-point literals
--

signed_float_literal :: { Contextual (Integer, Integer) }:
    float_literal                           { $1 }
  | "-" float_literal                       { context $1 <@@ (negateFloat <\$> $2) }

float_literal :: { Contextual (Integer, Integer) }:
    float_token                             { floatTokenValue <\$> $1 }

signed_double_literal :: { Contextual (Integer, Integer) }:
    double_literal                          { $1 }
  | "-" double_literal                      { context $1 <@@ (negateFloat <\$> $2) }

double_literal :: { Contextual (Integer, Integer) }:
    double_token                            { floatTokenValue <\$> $1 }

--
-- boolean literals
--

boolean_literal :: { Contextual Bool }:
    "true"                                  { True  :@@ context $1 }
  | "false"                                 { False :@@ context $1 }

--
-- string literals
--

character_literal :: { Contextual Char }:
    char_token                              { charTokenValue <\$> $1 }

string_literal :: { Contextual ByteString }:
    string_token                            { stringTokenValue <\$> $1 }

symbol_literal :: { Contextual ByteString }:
    symbol_token                            { stringTokenValue <\$> $1 }

--
-- identifiers
--

op :: { Contextual Ident }:
    op_token                                { identTokenValue <\$> $1 }

varid :: { Contextual Ident }:
    varid_token                             { identTokenValue <\$> $1 }

plainid :: { Contextual Ident }:
    plainid_token                           { identTokenValue <\$> $1 }
  | varid                                   { $1 }
  | op                                      { $1 }

id :: { Contextual Ident }:
    plainid                                 { $1 }
  | stringid_token                          { identTokenValue <\$> $1 }


qual_id :: { Contextual QualId }:
    id                                      { PHead $1 :@@ between $1 $1 }
  | qual_id "." id                          { (value $1 ::: $3) :@@ between $1 $3 }

ids :: { Contextual Ids }:
    id                                      { PHead $1 :@@ between $1 $1 }
  | qual_id "," id                          { (value $1 ::: $3) :@@ between $1 $3 }


path :: { Contextual Path }:
    stable_id                               { Path_StableId $1 <\$ $1 }
  | stable_id_prefix_opt "this"                           { Path_This (Just $1) <\$ $1 }

stable_id :: { Contextual StableId }:
    id stable_id_suffix_opt                 { error "TODO" }
  | stable_id_prefix_opt "this" stable_id_suffix          { error "TODO" }
  | stable_id_prefix_opt "super" class_qualifier_opt stable_id_suffix { error "TODO" }

stable_id_prefix_opt:
    stable_id_prefix                        { ... }
  | {- empty -}                             { ... }

stable_id_prefix:
    id "."                                  { ... }

stable_id_suffix_opt:
    stable_id_suffix                        { ... }
  | {- empty -}                             { ... }

stable_id_suffix:
    "." id stable_id_suffix_opt             { ... }

class_qualifier_opt :: { Maybe (Contextual ClassQualifier) }:
    class_qualifier                         { Just $1 }
  | {- empty -}                             { Nothing }

class_qualifier :: { Contextual ClassQualifier }:
    "[" id "]"                              { ClassQualifier <\$> $2 }

compilation_unit :: { Contextual Path }:
    path { $1 }

{

newtype Grammar a = G { unwrap :: G a }
type G a = Tokens -> List PosError -> (Maybe a, List PosError)
type PosError = Positioned Error

instance Monad Grammar where
  m >>= k = G $ bindG m k
  return x = G $ \ ts es -> (Just x, es)

bindG :: Grammar a -> (a -> Grammar b) -> G b
bindG m k ts es = (my, es1)
  where
    (mx, es1) = unwrap m ts es2
    (my, es2) = maybe (Nothing, es) (\x -> unwrap (k x) ts es) mx

lexer :: (Either Position (Contextual Token) -> Grammar a) -> Grammar a
lexer k = G act
  where
    act (t ::> ts)        es = unwrap (k $ Right t) ts es
    act (e ::! ts)        es = let (mx, es') = act ts es in (mx, e:es')
    act eof@(EndTokens p) es = unwrap (k (Left p)) eof es

panic :: PosError -> Grammar a
panic e = G $ \ ts es -> (Nothing, [e])

syntaxError :: Either Position (Contextual Token) -> Grammar a
syntaxError t = panic ("Syntax error" :@ either position position t)

parseWith :: Grammar a -> Tokens -> Either (List PosError) a
parseWith g ts = maybe (Left es) (\r -> if null es then Right r else Left es) mr
  where
    (mr, es) = unwrap g ts []

}

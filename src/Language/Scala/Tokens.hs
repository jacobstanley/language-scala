-- Data type used to describe individual lexical tokens, as well
-- as sequences of such tokens. A token sequence may include embedded
-- error messages, and always ends in a special "EndTokens" element.
-- All tokens in a sequence are annotated with their source context,
-- and "EndTokens" is annotated with position information.

module Language.Scala.Tokens
    ( Token (..)
    , Tokens (..)
    , NewLineCount (..)

    , applyNewLineRules

    , tokenLexeme
    , quotedString
    , normalisedScientific
    ) where

------------------------------------------------------------------------

import           Data.Bits ((.&.), shiftR)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Char (ord)
import           Data.List (intersperse)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.Word (Word8)

import           Language.Scala.Context
import           Language.Scala.Position
import           Language.Scala.Util

------------------------------------------------------------------------

infixr 5 ::>, ::!

------------------------------------------------------------------------

data NewLineCount = One | Many
    deriving (Eq, Ord, Show)

-- | A Scala token.
data Token =

-- New-line, we need to track this differently from other whitespace as we need
-- to deal with it specially in a pre-parse phase:

      Tok_NewLine  !NewLineCount

-- Identifiers:

    | Tok_VarId    !Ident
    | Tok_PlainId  !Ident
    | Tok_StringId !Ident

-- Integers:

    | Tok_Int      !Integer
    | Tok_Long     !Integer

-- Rationals are stored as integer mantissa and decimal exponent,
-- normalised to minimise the mantissa's value:

    | Tok_Float    !Integer !Integer
    | Tok_Double   !Integer !Integer

-- Strings are stored as raw binary data, with quotes removed
-- and escape sequences replaced by their constituent
-- characters:

    | Tok_Char     !Char
    | Tok_String   !ByteString

-- Symbol literals are shorthand for scala.Symbol("x")

    | Tok_Symbol   !ByteString

-- Keywords:

    | Tok_Abstract  | Tok_Case    | Tok_Catch
    | Tok_Class     | Tok_Def     | Tok_Do
    | Tok_Else      | Tok_Extends | Tok_False
    | Tok_Final     | Tok_Finally | Tok_For
    | Tok_ForSome   | Tok_If      | Tok_Implicit
    | Tok_Import    | Tok_Lazy    | Tok_Match
    | Tok_New       | Tok_Null    | Tok_Object
    | Tok_Override  | Tok_Package | Tok_Private
    | Tok_Protected | Tok_Return  | Tok_Sealed
    | Tok_Super     | Tok_This    | Tok_Throw
    | Tok_Trait     | Tok_Try     | Tok_True
    | Tok_Type      | Tok_Val     | Tok_Var
    | Tok_While     | Tok_With    | Tok_Yield

-- Symbols:

    | Tok_LParen   | Tok_RParen
    | Tok_LBracket | Tok_RBracket
    | Tok_LBrace   | Tok_RBrace

    | Tok_Dot        | Tok_Comma      | Tok_Semi
    | Tok_Underscore | Tok_Colon      | Tok_Equals
    | Tok_Arrow      | Tok_BackArrow  | Tok_LowerBound
    | Tok_ViewBound  | Tok_UpperBound | Tok_Projection
    | Tok_Annotation

    deriving (Eq, Ord)

------------------------------------------------------------------------

-- The "show" instance formats each token's canonical lexeme in
-- angle brackets, and formats a list of tokens as space-separated
-- sentence:

instance Show Token where
  showsPrec _ t = showChar '<' . showByteString (tokenLexeme t) . showChar '>'
  showList = foldl (.) id . intersperse (showChar ' ') . map shows

------------------------------------------------------------------------

-- | A list of tokens annotated with context information, intermixed
-- with error messages and ending in an explicit "EndTokens"
-- marker, used to represent the output of lexical analyser.

data Tokens =
    Contextual Token ::> Tokens
  | Positioned Error ::! Tokens
  | EndTokens !Position
  deriving (Show)

------------------------------------------------------------------------

-- | Extracts the canonical lexeme of every token; this isn't necessarily
-- identical to what appeared in the source file, but is semantically
-- equivalent. In particular, all integers end up in decimal, all
-- rationals end up in their minimal scientific notation with lower-case
-- exponent, and strings/chars end up with all non-printable characters
-- escaped using either symbolic or minimal octal escape sequences.

tokenLexeme :: Token -> ByteString
tokenLexeme t = case t of
  Tok_NewLine One  -> fromString "newline"
  Tok_NewLine Many -> fromString "newlines"
  Tok_VarId    x   -> x
  Tok_PlainId  x   -> x
  Tok_StringId x   -> quotedString '`' x
  Tok_Symbol   x   -> fromString "\'" <> x
  Tok_Int      x   -> fromString (shows x "")
  Tok_Long     x   -> fromString (shows x "L")
  Tok_Float  m e   -> fromString (uncurry showScientific (normalisedScientific m e) "F")
  Tok_Double m e   -> fromString (uncurry showScientific (normalisedScientific m e) "")
  Tok_Char     x   -> quotedString '\'' (C.pack [x])
  Tok_String   x   -> quotedString '"' x
  Tok_Abstract     -> fromString "abstract"
  Tok_Case         -> fromString "case"
  Tok_Catch        -> fromString "catch"
  Tok_Class        -> fromString "class"
  Tok_Def          -> fromString "def"
  Tok_Do           -> fromString "do"
  Tok_Else         -> fromString "else"
  Tok_Extends      -> fromString "extends"
  Tok_False        -> fromString "false"
  Tok_Final        -> fromString "final"
  Tok_Finally      -> fromString "finally"
  Tok_For          -> fromString "for"
  Tok_ForSome      -> fromString "forSome"
  Tok_If           -> fromString "if"
  Tok_Implicit     -> fromString "implicit"
  Tok_Import       -> fromString "import"
  Tok_Lazy         -> fromString "lazy"
  Tok_Match        -> fromString "match"
  Tok_New          -> fromString "new"
  Tok_Null         -> fromString "null"
  Tok_Object       -> fromString "object"
  Tok_Override     -> fromString "override"
  Tok_Package      -> fromString "package"
  Tok_Private      -> fromString "private"
  Tok_Protected    -> fromString "protected"
  Tok_Return       -> fromString "return"
  Tok_Sealed       -> fromString "sealed"
  Tok_Super        -> fromString "super"
  Tok_This         -> fromString "this"
  Tok_Throw        -> fromString "throw"
  Tok_Trait        -> fromString "trait"
  Tok_Try          -> fromString "try"
  Tok_True         -> fromString "true"
  Tok_Type         -> fromString "type"
  Tok_Val          -> fromString "val"
  Tok_Var          -> fromString "var"
  Tok_While        -> fromString "while"
  Tok_With         -> fromString "with"
  Tok_Yield        -> fromString "yield"
  Tok_LParen       -> fromString "("
  Tok_RParen       -> fromString ")"
  Tok_LBracket     -> fromString "["
  Tok_RBracket     -> fromString "]"
  Tok_LBrace       -> fromString "{"
  Tok_RBrace       -> fromString "}"
  Tok_Dot          -> fromString "."
  Tok_Comma        -> fromString ","
  Tok_Semi         -> fromString ";"
  Tok_Underscore   -> fromString "_"
  Tok_Colon        -> fromString ":"
  Tok_Equals       -> fromString "="
  Tok_Arrow        -> fromString "=>"
  Tok_BackArrow    -> fromString "<-"
  Tok_LowerBound   -> fromString "<:"
  Tok_ViewBound    -> fromString "<%"
  Tok_UpperBound   -> fromString ">:"
  Tok_Projection   -> fromString "#"
  Tok_Annotation   -> fromString "@"

------------------------------------------------------------------------

-- | Normalise a scientific mantissa and exponent.
normalisedScientific :: Integer -> Integer -> (Integer, Integer)
normalisedScientific 0 _ = (0, 0)
normalisedScientific m e | m < 0 = (negate m', e')
 where (m', e') = normalisedScientific (negate m) e
normalisedScientific m e | m `mod` 10 == 0 = normalisedScientific (m `div` 10) (e + 1)
normalisedScientific m e = (m, e)

-- | Format a rational using standard scientific notation.
showScientific :: Integer -> Integer -> ShowS
showScientific m e
    | m == 0    = showString "0.0e0"
    | m < 0     = showChar '-' . showScientific (negate m) e
    | otherwise = showChar is
                . showChar '.'
                . showString (if null fs then "0" else fs)
                . showChar 'e'
                . shows (e + fromIntegral (length fs))
  where
    (is:fs) = show m

------------------------------------------------------------------------

-- | Quote and escape string literals.
quotedString :: Char -> ByteString -> ByteString
quotedString qc x = B.pack $ (quoteChar qc :) $ quoteBytes $ B.unpack x
  where
    quoteBytes (0x08:bs) = quoteChar '\\' : quoteChar 'b'  : quoteBytes bs
    quoteBytes (0x09:bs) = quoteChar '\\' : quoteChar 't'  : quoteBytes bs
    quoteBytes (0x0A:bs) = quoteChar '\\' : quoteChar 'n'  : quoteBytes bs
    quoteBytes (0x0C:bs) = quoteChar '\\' : quoteChar 'f'  : quoteBytes bs
    quoteBytes (0x0D:bs) = quoteChar '\\' : quoteChar 'r'  : quoteBytes bs
    quoteBytes (0x22:bs) = quoteChar '\\' : quoteChar '\"' : quoteBytes bs
    quoteBytes (0x27:bs) = quoteChar '\\' : quoteChar '\'' : quoteBytes bs
    quoteBytes (0x5C:bs) = quoteChar '\\' : quoteChar '\\' : quoteBytes bs
    quoteBytes (b:bs)
      | 32 <= b && b <= 126 = b : quoteBytes bs
      | otherwise           = quoteChar '\\' : quoteOctal b bs
    quoteBytes [] = [quoteChar qc]

    quoteOctal b bs
      | b > 0o77 || startsWithOctit bs = quoteOctit (b `shiftR` 6)
                                       : quoteOctit ((b `shiftR` 3) .&. 7)
                                       : quoteOctit (b .&. 7)
                                       : quoteBytes bs

      | b > 0o7                        = quoteOctit (b `shiftR` 3)
                                       : quoteOctit (b .&. 0o7)
                                       : quoteBytes bs

      | otherwise                      = quoteOctit b
                                       : quoteBytes bs

    startsWithOctit (b:_) = quoteChar '0' <= b && b <= quoteChar '7'
    startsWithOctit [] = False

    quoteOctit :: Word8 -> Word8
    quoteOctit = (+ quoteChar '0')

    quoteChar :: Char -> Word8
    quoteChar = fromIntegral . ord

------------------------------------------------------------------------

-- | Apply Scala's special newline rules. sometimes a newline should be
-- interpreted as whitespace, and sometimes as a special token.
applyNewLineRules :: Tokens -> Tokens
applyNewLineRules = updateState [] . mergeNewLines
  where
    mergeNewLines :: Tokens -> Tokens
    mergeNewLines ts = case ts of

      (Tok_NewLine _ :@@ lc ::>
       Tok_NewLine _ :@@ tc ::> ts') ->
        mergeNewLines (Tok_NewLine Many :@@ lc @@> tc ::> ts')

      t ::> ts' -> t ::> mergeNewLines ts'
      _         -> ts


    updateState :: [Token] -> Tokens -> Tokens
    updateState ss ts = case (ss, ts) of
      (Tok_LParen   : ss', Tok_RParen   :@@ _ ::> _) -> apply ss' ts
      (Tok_LBracket : ss', Tok_RBracket :@@ _ ::> _) -> apply ss' ts
      (Tok_LBrace   : ss', Tok_RBrace   :@@ _ ::> _) -> apply ss' ts
      (Tok_Case     : ss', Tok_Arrow    :@@ _ ::> _) -> apply ss' ts

      (                 _, Tok_LParen   :@@ _ ::> _) -> apply (Tok_LParen   : ss) ts
      (                 _, Tok_LBracket :@@ _ ::> _) -> apply (Tok_LBracket : ss) ts
      (                 _, Tok_LBrace   :@@ _ ::> _) -> apply (Tok_LBrace   : ss) ts

      (_, Tok_Case :@@ _ ::> Tok_Class  :@@ _ ::> _) -> apply ss ts
      (_, Tok_Case :@@ _ ::> Tok_Object :@@ _ ::> _) -> apply ss ts
      (_, Tok_Case :@@ _ ::>                _ ::> _) -> apply (Tok_Case     : ss) ts

      (_,                                         _) -> apply ss ts


    newlinesEnabled :: [Token] -> Bool
    newlinesEnabled (Tok_LParen   : _) = False
    newlinesEnabled (Tok_LBracket : _) = False
    newlinesEnabled (Tok_Case     : _) = False
    newlinesEnabled (               _) = True


    apply :: [Token] -> Tokens -> Tokens

    apply s (pre ::> nl@(Tok_NewLine _ :@@ _)
                 ::> ts@(Tok_Case      :@@ _ ::> Tok_Class :@@ _ ::> _))
        | stmtEnd pre &&
          newlinesEnabled s = pre ::> nl ::> updateState s ts

    apply s (pre ::> nl@(Tok_NewLine _ :@@ _)
                 ::> ts@(Tok_Case      :@@ _ ::> Tok_Object :@@ _ ::> _))
        | stmtEnd pre &&
          newlinesEnabled s = pre ::> nl ::> updateState s ts

    apply s (pre ::> nl@(Tok_NewLine _ :@@ _)
                 ::> ts@(post ::> _))
        | stmtEnd   pre  &&
          stmtBegin post &&
          newlinesEnabled s = pre ::> nl ::> updateState s ts

    apply s (Tok_NewLine _ :@@ _ ::> ts) = updateState s ts

    apply s (t ::> ts) = t ::> updateState s ts

    apply _ ts = ts


stmtEnd :: Contextual Token -> Bool
stmtEnd ct = case value ct of
  Tok_VarId    _ -> True
  Tok_PlainId  _ -> True
  Tok_StringId _ -> True
  Tok_Int      _ -> True
  Tok_Long     _ -> True
  Tok_Float  _ _ -> True
  Tok_Double _ _ -> True
  Tok_Char     _ -> True
  Tok_String   _ -> True
  Tok_This       -> True
  Tok_Null       -> True
  Tok_True       -> True
  Tok_False      -> True
  Tok_Return     -> True
  Tok_Type       -> True
  Tok_Underscore -> True
  Tok_RParen     -> True
  Tok_RBracket   -> True
  Tok_RBrace     -> True
  _              -> False

stmtBegin :: Contextual Token -> Bool
stmtBegin ct = case value ct of
  Tok_Case       -> False
  Tok_Catch      -> False
  Tok_Else       -> False
  Tok_Extends    -> False
  Tok_Finally    -> False
  Tok_ForSome    -> False
  Tok_Match      -> False
  Tok_With       -> False
  Tok_Yield      -> False
  Tok_Comma      -> False
  Tok_Dot        -> False
  Tok_Semi       -> False
  Tok_Colon      -> False
  Tok_Equals     -> False
  Tok_Arrow      -> False
  Tok_BackArrow  -> False
  Tok_LowerBound -> False
  Tok_ViewBound  -> False
  Tok_UpperBound -> False
  Tok_Projection -> False
  Tok_LBracket   -> False
  Tok_RParen     -> False
  Tok_RBracket   -> False
  Tok_RBrace     -> False
  _              -> True

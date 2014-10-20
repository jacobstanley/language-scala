-- Data type used to describe individual lexical tokens, as well
-- as sequences of such tokens. A token sequence may include embedded
-- error messages, and always ends in a special "Tok_EOF" element.
-- All tokens in a sequence are annotated with their source context,
-- and "Tok_EOF" is annotated with position information.

module Language.Scala.Tokens
    ( Token (..)
    , Tokens (..)

    , tokenLexeme
    , quotedString
    , normalisedScientific
    ) where

------------------------------------------------------------------------

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Char
import           Data.List
import           Data.String
import           Data.Word

import Language.Scala.Contexts
import Language.Scala.Positions
import Language.Scala.Utilities

------------------------------------------------------------------------

infixr 5 ::>, ::!

------------------------------------------------------------------------

-- | A Scala token.
data Token =

-- Identifiers:

      Tok_VarId    !Name
    | Tok_PlainId  !Name
    | Tok_StringId !Name

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

    | Tok_Char     !ByteString
    | Tok_String   !ByteString

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

    | Tok_Dot        | Tok_Comma      | Tok_Semicolon
    | Tok_Underscore | Tok_Colon      | Tok_Equals
    | Tok_Arrow      | Tok_BackArrow  | Tok_LowerBound
    | Tok_ViewBound  | Tok_UpperBound | Tok_Projection
    | Tok_Annotation

-- New-line, we need to track this differently from other whitespace as we need
-- to deal with it specially in a pre-parse phase:

    | Tok_NewLine

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
    Contextual Token  ::> Tokens
  | Positioned String ::! Tokens
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

tokenLexeme (Tok_VarId x)    = x
tokenLexeme (Tok_PlainId x)  = x
tokenLexeme (Tok_StringId x) = quotedString '`' x

tokenLexeme (Tok_Int x)      = fromString $ shows x ""
tokenLexeme (Tok_Long x)     = fromString $ shows x "L"

tokenLexeme (Tok_Float m e)  = fromString $ uncurry showScientific (normalisedScientific m e) "F"
tokenLexeme (Tok_Double m e) = fromString $ uncurry showScientific (normalisedScientific m e) ""

tokenLexeme (Tok_Char x)     = quotedString '\'' x
tokenLexeme (Tok_String x)   = quotedString '"' x

tokenLexeme (Tok_Abstract)   = fromString "abstract"
tokenLexeme (Tok_Case)       = fromString "case"
tokenLexeme (Tok_Catch)      = fromString "catch"
tokenLexeme (Tok_Class)      = fromString "class"
tokenLexeme (Tok_Def)        = fromString "def"
tokenLexeme (Tok_Do)         = fromString "do"
tokenLexeme (Tok_Else)       = fromString "else"
tokenLexeme (Tok_Extends)    = fromString "extends"
tokenLexeme (Tok_False)      = fromString "false"
tokenLexeme (Tok_Final)      = fromString "final"
tokenLexeme (Tok_Finally)    = fromString "finally"
tokenLexeme (Tok_For)        = fromString "for"
tokenLexeme (Tok_ForSome)    = fromString "forSome"
tokenLexeme (Tok_If)         = fromString "if"
tokenLexeme (Tok_Implicit)   = fromString "implicit"
tokenLexeme (Tok_Import)     = fromString "import"
tokenLexeme (Tok_Lazy)       = fromString "lazy"
tokenLexeme (Tok_Match)      = fromString "match"
tokenLexeme (Tok_New)        = fromString "new"
tokenLexeme (Tok_Null)       = fromString "null"
tokenLexeme (Tok_Object)     = fromString "object"
tokenLexeme (Tok_Override)   = fromString "override"
tokenLexeme (Tok_Package)    = fromString "package"
tokenLexeme (Tok_Private)    = fromString "private"
tokenLexeme (Tok_Protected)  = fromString "protected"
tokenLexeme (Tok_Return)     = fromString "return"
tokenLexeme (Tok_Sealed)     = fromString "sealed"
tokenLexeme (Tok_Super)      = fromString "super"
tokenLexeme (Tok_This)       = fromString "this"
tokenLexeme (Tok_Throw)      = fromString "throw"
tokenLexeme (Tok_Trait)      = fromString "trait"
tokenLexeme (Tok_Try)        = fromString "try"
tokenLexeme (Tok_True)       = fromString "true"
tokenLexeme (Tok_Type)       = fromString "type"
tokenLexeme (Tok_Val)        = fromString "val"
tokenLexeme (Tok_Var)        = fromString "var"
tokenLexeme (Tok_While)      = fromString "while"
tokenLexeme (Tok_With)       = fromString "with"
tokenLexeme (Tok_Yield)      = fromString "yield"

tokenLexeme (Tok_LParen)     = fromString "("
tokenLexeme (Tok_RParen)     = fromString ")"
tokenLexeme (Tok_LBracket)   = fromString "["
tokenLexeme (Tok_RBracket)   = fromString "]"
tokenLexeme (Tok_LBrace)     = fromString "{"
tokenLexeme (Tok_RBrace)     = fromString "}"

tokenLexeme (Tok_Dot)        = fromString "."
tokenLexeme (Tok_Comma)      = fromString ","
tokenLexeme (Tok_Semicolon)  = fromString ";"

tokenLexeme (Tok_Underscore) = fromString "_"
tokenLexeme (Tok_Colon)      = fromString ":"
tokenLexeme (Tok_Equals)     = fromString "="
tokenLexeme (Tok_Arrow)      = fromString "=>"
tokenLexeme (Tok_BackArrow)  = fromString "<-"
tokenLexeme (Tok_LowerBound) = fromString "<:"
tokenLexeme (Tok_ViewBound)  = fromString "<%"
tokenLexeme (Tok_UpperBound) = fromString ">:"
tokenLexeme (Tok_Projection) = fromString "#"
tokenLexeme (Tok_Annotation) = fromString "@"

tokenLexeme (Tok_NewLine)    = fromString "new-line"

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
    | m == 0    = showString "0.0E0"
    | m < 0     = showChar '-' . showScientific (negate m) e
    | otherwise = showChar is
                . showChar '.'
                . showString (if null fs then "0" else fs)
                . showChar 'E'
                . shows (e + fromIntegral (length fs))
  where
    (is:fs) = show m

------------------------------------------------------------------------

-- | Quote and escape string literals.
quotedString :: Char -> ByteString -> ByteString
quotedString qc x = ByteString.pack $ (quoteChar qc :) $ quoteBytes $ ByteString.unpack x
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
  quoteBytes [] = [quoteChar '\"']

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

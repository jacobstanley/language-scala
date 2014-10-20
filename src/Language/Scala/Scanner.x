{

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-unused-matches     #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}

-- Alex scanner for .scala files, with minor generalisations.
-- The output is a sequence of tokens and error messages defined
-- in the "Tokens" module, annotated with source code context
-- defined in "Contexts".

module Language.Scala.Scanner
    ( Tokens
    , scanTokens
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Char
import           Data.Functor
import           Data.Maybe
import           Data.Word
import           Numeric

import           Language.Scala.Contexts
import           Language.Scala.Positions
import           Language.Scala.Tokens
import           Language.Scala.Utilities

}

$printable                    = [ \ - \~ ]
$space                        = [ \ \t \r ]

$upper                        = [ A-Z \$ _ ]
$lower                        = [ a-z ]
@letter                       = $upper | $lower
$opchar                       = [ \! \# \% \& \* \+ \- \/ \< \= \> \? \@ \| \~ ]

$octit                        = [ 0-7 ]
$digit                        = [ 0-9 ]
$non_zero_digit               = [ 1-9 ]
$hexit                        = [ 0-9 a-f A-F ]

@op                           = $opchar+
@idrest                       = (@letter | $digit)* (_ @op)?
@varid                        = $lower @idrest
@plainid                      = $upper @idrest | @varid | @op

@symbol_literal               = \' @plainid

@integer_type                 = [ L l ]
@decimal_literal              = (0 | $non_zero_digit $digit*) @integer_type?
@hexadecimal_literal          = (0x $hexit+)                  @integer_type?

@float_type                   = [ F f D d ]
@exponent                     = [ E e ] [ \+ \- ]? $digit+
@rational_literal             = $digit* \. $digit+ @exponent? @float_type?
                              |            $digit+ @exponent  @float_type?
                              |            $digit+ @exponent? @float_type

@space                        = $space+
@newline                      = \n
@line_comment                 = "//" [^ \n ]* @newline?
@block_comment                = "/*" (("/"+ | "*"+)? ([^ \* \/ \n ] | @newline))* "*"* "*/"
@whitespace                   = (@space | @line_comment | @block_comment)+

tokens :-

<0> @whitespace               ;
<0> @newline                  { produceSymbol Tok_NewLine    }

<0> "("                       { produceSymbol Tok_LParen     }
<0> ")"                       { produceSymbol Tok_RParen     }
<0> "["                       { produceSymbol Tok_LBracket   }
<0> "]"                       { produceSymbol Tok_RBracket   }
<0> "{"                       { produceSymbol Tok_LBrace     }
<0> "}"                       { produceSymbol Tok_RBrace     }

<0> "."                       { produceSymbol Tok_Dot        }
<0> ";"                       { produceSymbol Tok_Semicolon  }
<0> ","                       { produceSymbol Tok_Comma      }

<0> "_"                       { produceSymbol Tok_Underscore }
<0> ":"                       { produceSymbol Tok_Colon      }
<0> "="                       { produceSymbol Tok_Equals     }
<0> "=>"                      { produceSymbol Tok_Arrow      }
<0> "<-"                      { produceSymbol Tok_BackArrow  }
<0> "<:"                      { produceSymbol Tok_LowerBound }
<0> "<%"                      { produceSymbol Tok_ViewBound  }
<0> ">:"                      { produceSymbol Tok_UpperBound }
<0> "#"                       { produceSymbol Tok_Projection }
<0> "@"                       { produceSymbol Tok_Annotation }

-- The Unicode operators \u2  1D2 ‘⇒’ and \u2190 ‘←’, which have the ASCII
-- equivants '=>' and '<-',   are also reserved.

<0> "abstract"                { produceSymbol Tok_Abstract   }
<0> "case"                    { produceSymbol Tok_Case       }
<0> "catch"                   { produceSymbol Tok_Catch      }
<0> "class"                   { produceSymbol Tok_Class      }
<0> "def"                     { produceSymbol Tok_Def        }
<0> "do"                      { produceSymbol Tok_Do         }
<0> "else"                    { produceSymbol Tok_Else       }
<0> "extends"                 { produceSymbol Tok_Extends    }
<0> "false"                   { produceSymbol Tok_False      }
<0> "final"                   { produceSymbol Tok_Final      }
<0> "finally"                 { produceSymbol Tok_Finally    }
<0> "for"                     { produceSymbol Tok_For        }
<0> "forSome"                 { produceSymbol Tok_ForSome    }
<0> "if"                      { produceSymbol Tok_If         }
<0> "implicit"                { produceSymbol Tok_Implicit   }
<0> "import"                  { produceSymbol Tok_Import     }
<0> "lazy"                    { produceSymbol Tok_Lazy       }
<0> "match"                   { produceSymbol Tok_Match      }
<0> "new"                     { produceSymbol Tok_New        }
<0> "null"                    { produceSymbol Tok_Null       }
<0> "object"                  { produceSymbol Tok_Object     }
<0> "override"                { produceSymbol Tok_Override   }
<0> "package"                 { produceSymbol Tok_Package    }
<0> "private"                 { produceSymbol Tok_Private    }
<0> "protected"               { produceSymbol Tok_Protected  }
<0> "return"                  { produceSymbol Tok_Return     }
<0> "sealed"                  { produceSymbol Tok_Sealed     }
<0> "super"                   { produceSymbol Tok_Super      }
<0> "this"                    { produceSymbol Tok_This       }
<0> "throw"                   { produceSymbol Tok_Throw      }
<0> "trait"                   { produceSymbol Tok_Trait      }
<0> "try"                     { produceSymbol Tok_Try        }
<0> "true"                    { produceSymbol Tok_True       }
<0> "type"                    { produceSymbol Tok_Type       }
<0> "val"                     { produceSymbol Tok_Val        }
<0> "var"                     { produceSymbol Tok_Var        }
<0> "while"                   { produceSymbol Tok_While      }
<0> "with"                    { produceSymbol Tok_With       }
<0> "yield"                   { produceSymbol Tok_Yield      }

<0> @varid                    { produceToken Tok_VarId   $ ByteString.take }
<0> @plainid                  { produceToken Tok_PlainId $ ByteString.take }

<0> @decimal_literal          { produceToken id $ readDecimal       }
<0> @hexadecimal_literal      { produceToken id $ readHexadecimal   }
<0> @rational_literal         { produceToken id $ readFloatingPoint }

<0> \"                        { beginString sq }
<0> \'                        { beginString sa }
<0> \`                        { beginString sb }

-- Should everything be legal inside strings except the quote itself?

<sq> [ $printable # [\"\\] ]+ { produceStringPart }
<sa> [ $printable # [\'\\] ]+ { produceStringPart }
<sb> [ $printable # [\`\\] ]+ { produceStringPart }

<sq,sa,sb> \\ b               { produceSimpleEscape '\b' }
<sq,sa,sb> \\ t               { produceSimpleEscape '\t' }
<sq,sa,sb> \\ n               { produceSimpleEscape '\n' }
<sq,sa,sb> \\ f               { produceSimpleEscape '\f' }
<sq,sa,sb> \\ r               { produceSimpleEscape '\r' }
<sq,sa,sb> \\ \"              { produceSimpleEscape '\"' }
<sq,sa,sb> \\ \'              { produceSimpleEscape '\'' }
<sq,sa,sb> \\ \\              { produceSimpleEscape '\\' }
<sq,sa,sb> \\ $octit{1,3}     { produceOctalEscape       }

<sq,sa,sb> \\                 { reportIllegalEscape           }
<sq,sa,sb> @newline           { reportIncompleteStringLiteral }

<sq> \"                       { endString     }
<sa> \'                       { endChar       }
<sb> \`                       { endStringId   }

{

-- Alex input is simply a positioned byte string:
type AlexInput = Positioned ByteString

-- For simplicity, we keep track of character positions before handing them off
-- to Alex and take the opportunity to normalise all line breaks including
-- CR+LF sequences into LFs:
alexGetByte (bs :@ p) = do
  (x, bs') <- ByteString.uncons bs
  case x of
    0x0D -> case ByteString.uncons bs' of
              Just (0x0A, bs'') -> return (0x0A, bs'' :@ nextLine p)
              _                 -> return (0x0A, bs'  :@ nextLine p)
    0x0A -> return (x, bs' :@ nextLine p)
    0x09 -> return (x, bs' :@ nextTab p)
    _    -> return (x, bs' :@ nextColumn p)

-- | Parses a positioned bytestring into a list tokens.
scanTokens :: Positioned ByteString -> Tokens
scanTokens inp =
  case alexScan inp 0 of
    AlexToken inp' len act -> act undefined 0 inp len inp'
    AlexSkip inp' _        -> scanTokens inp'
    AlexError inp'         -> fmap unexpected inp ::! scanTokens inp'
    AlexEOF                -> EndTokens $ position inp

-- | Produce a human-readable error message.
unexpected :: ByteString -> String
unexpected bs = "Unexpected " ++ inputContext bs

-- | Establish human-readable input context.
inputContext :: ByteString -> String
inputContext bs = fromMaybe "end of file" $ do
  (x, _) <- UTF8.uncons bs
  return ("character " ++ show x)

-- | Produce a single token.
produceToken :: (a -> Token) -> (Int -> ByteString -> a) -> s -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
produceToken t p _ _ inp len inp' = (t $ p len $ value inp) :@@ between inp inp' ::> scanTokens inp'

parseOnly :: ReadS a -> Int -> ByteString -> a
parseOnly r len str = v
  where
    (v, "") = parseWith r len str

parseWith :: ReadS a -> Int -> ByteString -> (a, String)
parseWith r len str = v
  where
    [v] = r $ UTF8.toString $ ByteString.take len str

-- | Produce an int or a long token from a decimal literal.
readDecimal :: Int -> ByteString -> Token
readDecimal len str =
  case parseWith readDec len str of
    (v, "" ) -> Tok_Int  v
    (v, "l") -> Tok_Long v
    (v, "L") -> Tok_Long v

-- | Produce an int or a long token from a hexadecimal literal.
readHexadecimal :: Int -> ByteString -> Token
readHexadecimal len str =
  case parseWith (readHex . drop 2) len str of
    (v, "" ) -> Tok_Int  v
    (v, "l") -> Tok_Long v
    (v, "L") -> Tok_Long v

-- | Produce a double or a float token from a rational literal.
readFloatingPoint :: Int -> ByteString -> Token
readFloatingPoint len str =
  case parseWith readRational len str of
    ((m, e), "")  -> Tok_Double m e
    ((m, e), "d") -> Tok_Double m e
    ((m, e), "D") -> Tok_Double m e
    ((m, e), "f") -> Tok_Float  m e
    ((m, e), "F") -> Tok_Float  m e

-- | Reader for rational token values.
readRational :: ReadS (Integer, Integer)
readRational str = readIntegerPart 0 str
  where
    readIntegerPart m (c:xs)
      | ('0' <= c && c <= '9') = readIntegerPart (m * 10 + digitValue c) xs
      | (c == '.')             = readFractionPart m 0 xs
      | (c == 'e' || c == 'E') = readExponentPart m 0 xs
    readIntegerPart m xs       = [(normalisedScientific m 0, xs)]

    readFractionPart m e (c:xs)
      | ('0' <= c && c <= '9') = readFractionPart (m * 10 + digitValue c) (e - 1) xs
      | (c == 'e' || c == 'E') = readExponentPart m e xs
    readFractionPart m e xs    = [(normalisedScientific m e, xs)]

    readExponentPart m e ('-':xs) = readExponentValue m e (-1) 0 xs
    readExponentPart m e ('+':xs) = readExponentValue m e  (1) 0 xs
    readExponentPart m e     (xs) = readExponentValue m e  (1) 0 xs

    readExponentValue m e s e' (c:xs)
      | ('0' <= c && c <= '9')    = readExponentValue m e s (e' * 10 + digitValue c) xs
    readExponentValue m e s e' xs = [(normalisedScientific m (e + s * e'), xs)]

    digitValue c = fromIntegral (ord c - ord '0')

-- | Symbols are trivial, since they have no features other than their position
-- position.
produceSymbol :: Token -> s -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
produceSymbol t _ _ inp _ inp' = t :@@ between inp inp' ::> scanTokens inp'

-- | String literals are a little bit tricky, since, in the interest of sensible
-- error messages, the scanner needs to process escape sequences into the
-- string's ultimate form.  While the input is assumed to be UTF-8, the
-- resulting string is always binary, and all non-ASCII characters must be
-- represented using octal or hexadecimal escape sequences.  Non-ASCII Unicode
-- characters will, in the current version, result in errors since we don't
-- want to presume any specific encoding on the user's part.
beginString :: Int -> s -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
beginString sc _ _ inp _ inp' = scanString ([] <$ inp) sc inp'

scanString :: Positioned [ByteString] -> Int -> Positioned ByteString -> Tokens
scanString rs sc inp =
  case alexScan inp sc of
    AlexToken inp' len act -> act rs sc inp len inp'
    AlexSkip inp' _        -> scanString rs sc inp'
    AlexError inp'         -> fmap unexpected inp ::! scanString rs sc inp'
    AlexEOF                -> reportIncompleteStringLiteral rs sc inp 0 inp

endString :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
endString rs _ _ _ inp' = (Tok_String $ ByteString.concat $ reverse $ value rs) :@@ between rs inp' ::> scanTokens inp'

endChar :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
endChar rs _ _ _ inp' = (Tok_Char $ ByteString.concat $ reverse $ value rs) :@@ between rs inp' ::> scanTokens inp'

endStringId :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
endStringId rs _ _ _ inp' = (Tok_StringId $ ByteString.concat $ reverse $ value rs) :@@ between rs inp' ::> scanTokens inp'

produceStringPart :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
produceStringPart rs sc inp len inp' = scanString (fmap (r :) rs) sc inp'
  where
    r = ByteString.take len (value inp)

produceSimpleEscape :: Char -> Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
produceSimpleEscape c rs sc _ _ inp' = scanString (fmap (r :) rs) sc inp'
  where
    r = ByteString.singleton $ fromIntegral $ ord c

produceOctalEscape :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
produceOctalEscape rs sc inp len inp'
    | x <= 0xFF = scanString (fmap (r :) rs) sc inp'
    | otherwise = ("Illegal octal escape sequence; every byte in a string literal must have a value less than 256" <$ inp) ::! scanString rs sc inp'
  where
    x = parseOnly (readOct . tail) len (value inp) :: Int
    r = ByteString.singleton $ fromIntegral x

reportIllegalEscape :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
reportIllegalEscape rs sc inp _ inp' = ("Illegal escape sequence" <$ inp) ::! scanString rs sc inp'

reportIncompleteStringLiteral :: Positioned [ByteString] -> Int -> Positioned ByteString -> Int -> Positioned ByteString -> Tokens
reportIncompleteStringLiteral _ _ inp _ inp' = ("Incomplete string literal" <$ inp) ::! scanTokens inp'

}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Data.Monoid ((<>))
import           System.Console.ANSI
import           System.Environment (getArgs)

import           Language.Scala.Parser
import           Language.Scala.Position
import           Language.Scala.Scanner
import           Language.Scala.Tokens
import           Language.Scala.Util

------------------------------------------------------------------------

main :: IO ()
main = do
    files <- getArgs
    mapM_ runScanner files

------------------------------------------------------------------------

runScanner :: FilePath -> IO ()
runScanner path = do
    bs <- B.readFile path

    let toks  = scanTokens (bs :@ (startPositionInFile path))
        toks' = applyNewLineRules toks

    mapM_ printColor (toList toks')
    putStrLn ""

    let ast = parseWith compilationUnitGrammar toks'
    print ast
  where
    toList :: Tokens -> [Token]
    toList (x ::> xs)    = value x : toList xs
    toList (_ ::! xs)    = toList xs
    toList (EndTokens _) = []

printColor :: Token -> IO ()
printColor tok = do
    setSGR . sgr . highlight $ tok
    B.putStr (lexeme tok)
    setSGR []
  where
    lexeme (Tok_NewLine One)  = "<nl>\n"
    lexeme (Tok_NewLine Many) = "<nl> <nl>\n"
    lexeme t                  = tokenLexeme t <> " "

------------------------------------------------------------------------

data Highlight = VarId
               | PlainId
               | StringId
               | Symbol
               | Operator
               | Number
               | String
               | Bracket
               | Reserved
               | ReservedOp
               | NewLine

sgr :: Highlight -> [SGR]
sgr h = case h of
    VarId         -> v Cyan
    PlainId       -> d Magenta
    StringId      -> d Magenta
    Symbol        -> v Yellow
    Operator      -> v White
    Number        -> v Red
    String        -> d Green
    Bracket       -> d Yellow
    Reserved      -> d Blue
    ReservedOp    -> v Blue
    NewLine       -> v Black
  where
    v c = [ SetColor Foreground Vivid c ]
    d c = [ SetColor Foreground Dull  c ]

highlight :: Token -> Highlight
highlight tok = case tok of
  Tok_NewLine  _ -> NewLine
  Tok_Op       _ -> Operator
  Tok_VarId    _ -> VarId
  Tok_PlainId  _ -> PlainId
  Tok_StringId _ -> StringId
  Tok_Symbol   _ -> Symbol
  Tok_Int      _ -> Number
  Tok_Long     _ -> Number
  Tok_Float  _ _ -> Number
  Tok_Double _ _ -> Number
  Tok_Char     _ -> String
  Tok_String   _ -> String
  Tok_Abstract   -> Reserved
  Tok_Case       -> Reserved
  Tok_Catch      -> Reserved
  Tok_Class      -> Reserved
  Tok_Def        -> Reserved
  Tok_Do         -> Reserved
  Tok_Else       -> Reserved
  Tok_Extends    -> Reserved
  Tok_False      -> Reserved
  Tok_Final      -> Reserved
  Tok_Finally    -> Reserved
  Tok_For        -> Reserved
  Tok_ForSome    -> Reserved
  Tok_If         -> Reserved
  Tok_Implicit   -> Reserved
  Tok_Import     -> Reserved
  Tok_Lazy       -> Reserved
  Tok_Match      -> Reserved
  Tok_New        -> Reserved
  Tok_Null       -> Reserved
  Tok_Object     -> Reserved
  Tok_Override   -> Reserved
  Tok_Package    -> Reserved
  Tok_Private    -> Reserved
  Tok_Protected  -> Reserved
  Tok_Return     -> Reserved
  Tok_Sealed     -> Reserved
  Tok_Super      -> Reserved
  Tok_This       -> Reserved
  Tok_Throw      -> Reserved
  Tok_Trait      -> Reserved
  Tok_Try        -> Reserved
  Tok_True       -> Reserved
  Tok_Type       -> Reserved
  Tok_Val        -> Reserved
  Tok_Var        -> Reserved
  Tok_While      -> Reserved
  Tok_With       -> Reserved
  Tok_Yield      -> Reserved
  Tok_LParen     -> Bracket
  Tok_RParen     -> Bracket
  Tok_LBracket   -> Bracket
  Tok_RBracket   -> Bracket
  Tok_LBrace     -> Bracket
  Tok_RBrace     -> Bracket
  Tok_Dot        -> ReservedOp
  Tok_Comma      -> ReservedOp
  Tok_Semi       -> ReservedOp
  Tok_Underscore -> ReservedOp
  Tok_Colon      -> ReservedOp
  Tok_Equals     -> ReservedOp
  Tok_Arrow      -> ReservedOp
  Tok_BackArrow  -> ReservedOp
  Tok_LowerBound -> ReservedOp
  Tok_ViewBound  -> ReservedOp
  Tok_UpperBound -> ReservedOp
  Tok_Projection -> ReservedOp
  Tok_Annotation -> ReservedOp

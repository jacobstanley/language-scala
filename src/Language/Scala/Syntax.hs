-- Raw parse tree of .scala files, devoid of any abstraction and used to isolate subsequent
-- validation and code generation stages from primitive parsing concerns.

module Language.Scala.Syntax where

import Data.ByteString (ByteString)

------------------------------------------------------------------------

data Literal =
      IntLiteral       !Integer
    | LongLiteral      !Integer
    | FloatLiteral     !Integer !Integer
    | DoubleLiteral    !Integer !Integer
    | BooleanLiteral   !Bool
    | CharacterLiteral !Char
    | StringLiteral    !ByteString
    | SymbolLiteral    !ByteString
    | NullLiteral
  deriving (Eq, Ord, Show)

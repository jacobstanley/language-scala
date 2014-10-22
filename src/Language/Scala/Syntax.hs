-- Raw parse tree of .scala files, devoid of any abstraction and used to isolate subsequent
-- validation and code generation stages from primitive parsing concerns.

module Language.Scala.Syntax where

import Data.ByteString (ByteString)

import Language.Scala.Context
import Language.Scala.Util

------------------------------------------------------------------------

data Literal =
      Lit_Int       !Integer
    | Lit_Long      !Integer
    | Lit_Float     !Integer !Integer
    | Lit_Double    !Integer !Integer
    | Lit_Boolean   !Bool
    | Lit_Character !Char
    | Lit_String    !ByteString
    | Lit_Symbol    !ByteString
    | Lit_Null
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------

type QualId = QList (Contextual Ident)

type Ids = QList (Contextual Ident)

------------------------------------------------------------------------

data Path =
      Path_StableId (Contextual StableId)
    | Path_This     (Maybe (Contextual Ident))
  deriving (Eq, Ord, Show)

data StableId =
      StableId_Id    (Contextual Ident)

    | StableId_Path  (Contextual Path)
                     (Contextual Ident)

    | StableId_Super (Maybe (Contextual Ident))
                     (Maybe (Contextual ClassQualifier))
                            (Contextual Ident)
  deriving (Eq, Ord, Show)

newtype ClassQualifier = ClassQualifier Ident
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------


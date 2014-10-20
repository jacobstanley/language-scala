-- A class used to keep track of file positions.

module Language.Scala.Positions
    ( Index
    , HasPosition (..)
    , Position
    , Positioned (..)

    , noPosition
    , lineNumber
    , columnNumber
    , startPositionInFile
    , advanceLine
    , advanceColumn
    , nextLine
    , nextColumn
    , nextTab
    , tabWidth
    ) where

------------------------------------------------------------------------

import Data.Functor
import Language.Scala.Utilities

------------------------------------------------------------------------

infixl 8 :@

------------------------------------------------------------------------

class HasPosition a where
  position :: a -> Position
  pmap     :: (Position -> Position) -> a -> a

------------------------------------------------------------------------

-- Note that we store line position separateley for efficiency,
-- since it changes much less often than column positions!

data Position = Pos LinePosition !Index deriving (Eq, Ord)
data LinePosition = LPos FilePath !Index deriving (Eq, Ord)

instance Show Position where
  showsPrec _ (Pos (LPos fp ln) cn) = showString fp . showLine
    where
      showLine   = if ln > 0 then showChar ':' . shows ln . showColumn else id
      showColumn = if cn > 0 then showChar ':' . shows cn else id

instance HasPosition Position where
  position = id
  pmap f = f

------------------------------------------------------------------------

data Positioned a = !a :@ !Position deriving (Eq, Ord, Show)

instance HasPosition (Positioned a) where
  position (_ :@ p) = p
  pmap f (x :@ p) = x :@ f p

instance HasValue Positioned where
  value (v :@ _) = v

instance Functor Positioned where
  fmap f (x :@ p) = f x :@ p
  x <$ (_ :@ p) = x :@ p

------------------------------------------------------------------------

noPosition :: Position
noPosition = Pos (LPos "" 0) 0

lineNumber :: Position -> Index
lineNumber (Pos (LPos _ ln) _) = ln

columnNumber :: Position -> Index
columnNumber (Pos (LPos _ _) cn) = cn

startPositionInFile :: FilePath -> Position
startPositionInFile fp = Pos (LPos fp 1) 1

advanceLine :: Index -> Position -> Position
advanceLine n (Pos (LPos fp ln) _) = Pos (LPos fp (ln + n)) 1

advanceColumn :: Index -> Position -> Position
advanceColumn n (Pos lp cn) = Pos lp (cn + n)

nextLine :: Position -> Position
nextLine = advanceLine 1

nextColumn :: Position -> Position
nextColumn = advanceColumn 1

nextTab :: Position -> Position
nextTab (Pos lp cn) = Pos lp ((cn - 1) `div` tabWidth * tabWidth + tabWidth + 1)

tabWidth :: Index
tabWidth = 8

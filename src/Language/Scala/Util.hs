module Language.Scala.Util
    ( HasValue (..)
    , List

    , Ident
    , Error
    , Index

    , PList (..)
    , fromPList

    , QList
    , fromQList
    , fromQListOpt

    , RList
    , fromRList

    , showByteString
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Maybe (fromMaybe)

------------------------------------------------------------------------

-- | For convenience, we define the class of "wrapped values", which provide a
-- "value" method with no particular semantic significance, apart of a
-- convenient name, other than the identity.
--
--   forall f . (f . value == value . fmap f)
--
class Functor f => HasValue f where
  value :: f a -> a

-- | I like prefix names for lists.
type List a = [a]

-- | Names and comments are represented by bytestrings.
type Ident = ByteString

-- | Error messages are represented by strings.
type Error = String

-- | A row number or column number.
type Index = Int

-- | Right-recursive lists with a distinguished leftmost element type,
-- specialised into non-empty lists ("QList") which start with "()" and
-- possibly-empty right-associative lists ("RList") in which the first
-- element has the same type as the rest of the list:
data PList a b = PHead !a | PList a b ::: b deriving (Eq, Ord)
type QList a   = PList a a
type RList a   = PList () a

instance (Show a, Show b) => Show (PList a b) where
  showsPrec _ (PHead  x) = shows x
  showsPrec _ (xs ::: x) = shows xs . showString " ::: " . shows x

fromPList :: PList a b -> (a, [b])
fromPList xs = fromPList' xs []
 where
  fromPList' (PHead  y) rs = (y, rs)
  fromPList' (ys ::: y) rs = fromPList' ys (y:rs)

fromQList :: QList a -> [a]
fromQList = uncurry (:) . fromPList

fromQListOpt :: Maybe (QList a) -> [a]
fromQListOpt = fromMaybe [] . fmap fromQList

fromRList :: RList a -> [a]
fromRList = snd . fromPList

-- | Show a byte string, like "showString"
showByteString :: ByteString -> ShowS
showByteString = showString . UTF8.toString

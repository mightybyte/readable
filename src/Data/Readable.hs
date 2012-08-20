module Data.Readable
  ( Readable(..)
  ) where

------------------------------------------------------------------------------
import           Data.ByteString.Char8 (ByteString)
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Text.Read
import           Data.Word


------------------------------------------------------------------------------
-- | Monadic analog to Read that uses ByteString instead of String.
class Readable a where
    fromText :: Monad m => Text -> m a
    fromBS   :: Monad m => ByteString -> m a
    fromBS = fromText . decodeUtf8


------------------------------------------------------------------------------
-- | Fails if the input wasn't parsed completely.
checkComplete :: Monad m => (t, Text) -> m t
checkComplete (a,rest)
  | T.null rest = return a
  | otherwise   = fail "Readable: could not parse completely"


instance Readable ByteString where
    fromText = return . encodeUtf8
instance Readable Text where
    fromText = return
instance Readable Int where
    fromText = either fail checkComplete . decimal
instance Readable Integer where
    fromText = either fail checkComplete . decimal
instance Readable Double where
    fromText = either fail checkComplete . double

instance Readable Int8 where
    fromText = either fail checkComplete . decimal
instance Readable Int16 where
    fromText = either fail checkComplete . decimal
instance Readable Int32 where
    fromText = either fail checkComplete . decimal
instance Readable Int64 where
    fromText = either fail checkComplete . decimal

instance Readable Word8 where
    fromText = either fail checkComplete . decimal
instance Readable Word16 where
    fromText = either fail checkComplete . decimal
instance Readable Word32 where
    fromText = either fail checkComplete . decimal
instance Readable Word64 where
    fromText = either fail checkComplete . decimal

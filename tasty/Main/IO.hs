-- |
-- Specific IO Actions
module Main.IO where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Text.Encoding as Text
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Main.DB as DB
import Main.Prelude hiding (isLeft, isRight)
import qualified Main.TextEncoder as TextEncoder
import qualified PostgreSQL.Binary.Decoding as A
import qualified PostgreSQL.Binary.Encoding as B

textRoundtrip :: LibPQ.Oid -> TextEncoder.Encoder a -> (Bool -> A.Value a) -> a -> IO (Either Text a)
textRoundtrip oid encoder decoder value =
  fmap (either (Left . Text.decodeUtf8) id)
    $ DB.session
    $ do
      integerDatetimes <- DB.integerDatetimes
      bytes <- DB.oneRow "SELECT $1" (params integerDatetimes) LibPQ.Binary
      return $ A.valueParser (decoder integerDatetimes) bytes
  where
    params integerDatetimes =
      [Just (oid, bytes, LibPQ.Text)]
      where
        bytes =
          (ByteStringLazy.toStrict . ByteStringBuilder.toLazyByteString . encoder) value

roundtrip :: LibPQ.Oid -> (Bool -> a -> B.Encoding) -> (Bool -> A.Value b) -> a -> IO (Either Text b)
roundtrip oid encoder decoder value =
  fmap (either (Left . Text.decodeUtf8) id)
    $ DB.session
    $ do
      integerDatetimes <- DB.integerDatetimes
      bytes <- DB.oneRow "SELECT $1" (params integerDatetimes) LibPQ.Binary
      return $ A.valueParser (decoder integerDatetimes) bytes
  where
    params integerDatetimes =
      [Just (oid, bytes, LibPQ.Binary)]
      where
        bytes =
          (B.encodingBytes . encoder integerDatetimes) value

parameterlessStatement :: ByteString -> (Bool -> A.Value a) -> a -> IO (Either Text a)
parameterlessStatement statement decoder value =
  fmap (either (Left . Text.decodeUtf8) id)
    $ DB.session
    $ do
      integerDatetimes <- DB.integerDatetimes
      bytes <- DB.oneRow statement [] LibPQ.Binary
      return $ A.valueParser (decoder integerDatetimes) bytes

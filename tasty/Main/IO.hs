-- |
-- Specific IO Actions
module Main.IO where

import Main.Prelude hiding (assert, isRight, isLeft)
import Test.QuickCheck
import Test.QuickCheck.Instances
import qualified Main.PTI as PTI
import qualified Main.DB as DB
import qualified Main.TextEncoder as TextEncoder 
import qualified Data.Scientific as Scientific
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import qualified Data.Text.Encoding as Text
import qualified PostgreSQL.Binary.Decoding as A
import qualified PostgreSQL.Binary.Encoding as B
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy as ByteStringLazy


textRoundtrip :: LibPQ.Oid -> TextEncoder.Encoder a -> (Bool -> A.Value a) -> a -> IO (Either Text a)
textRoundtrip oid encoder decoder value =
  fmap (either (Left . Text.decodeUtf8) id) $
  DB.session $ do
    integerDatetimes <- DB.integerDatetimes
    bytes <- DB.oneRow "SELECT $1" (params integerDatetimes) LibPQ.Binary
    return $ A.valueParser (decoder integerDatetimes) bytes
  where
    params integerDatetimes =
      [ Just ( oid , bytes , LibPQ.Text ) ]
      where
        bytes =
          (ByteStringLazy.toStrict . ByteStringBuilder.toLazyByteString . encoder) value

roundtrip :: LibPQ.Oid -> (Bool -> a -> B.Encoding) -> (Bool -> A.Value b) -> a -> IO (Either Text b)
roundtrip oid encoder decoder value =
  fmap (either (Left . Text.decodeUtf8) id) $
  DB.session $ do
    integerDatetimes <- DB.integerDatetimes
    bytes <- DB.oneRow "SELECT $1" (params integerDatetimes) LibPQ.Binary
    return $ A.valueParser (decoder integerDatetimes) bytes
  where
    params integerDatetimes =
      [ Just ( oid , bytes , LibPQ.Binary ) ]
      where
        bytes =
          (B.encodingBytes . encoder integerDatetimes) value

parameterlessStatement :: ByteString -> (Bool -> A.Value a) -> a -> IO (Either Text a)
parameterlessStatement statement decoder value =
  fmap (either (Left . Text.decodeUtf8) id) $
  DB.session $ do
    integerDatetimes <- DB.integerDatetimes
    bytes <- DB.oneRow statement [] LibPQ.Binary
    return $ A.valueParser (decoder integerDatetimes) bytes

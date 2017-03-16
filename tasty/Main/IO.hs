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
import qualified PostgreSQL.Binary.Data as Data
import qualified PostgreSQL.Binary.Encoding as Encoder
import qualified PostgreSQL.Binary.Decoder as Decoder
import qualified Database.PostgreSQL.LibPQ as LibPQ


textRoundtrip :: LibPQ.Oid -> TextEncoder.Encoder a -> (Bool -> Decoder.Decoder a) -> a -> IO (Either Text a)
textRoundtrip oid encoder decoder value =
  fmap (either (Left . Text.decodeUtf8) id) $
  DB.session $ do
    integerDatetimes <- DB.integerDatetimes
    bytes <- DB.oneRow "SELECT $1" (params integerDatetimes) LibPQ.Binary
    return $ Decoder.run (decoder integerDatetimes) bytes
  where
    params integerDatetimes =
      [ Just ( oid , bytes , LibPQ.Text ) ]
      where
        bytes =
          (convert . encoder) value

roundtrip :: LibPQ.Oid -> (Bool -> a -> Encoder.Value) -> (Bool -> Decoder.Decoder b) -> a -> IO (Either Text b)
roundtrip oid encoder decoder value =
  fmap (either (Left . Text.decodeUtf8) id) $
  DB.session $ do
    integerDatetimes <- DB.integerDatetimes
    bytes <- DB.oneRow "SELECT $1" (params integerDatetimes) LibPQ.Binary
    return $ Decoder.run (decoder integerDatetimes) bytes
  where
    params integerDatetimes =
      [ Just ( oid , bytes , LibPQ.Binary ) ]
      where
        bytes =
          (Encoder.valueBytes . encoder integerDatetimes) value

parameterlessStatement :: ByteString -> (Bool -> Decoder.Decoder a) -> a -> IO (Either Text a)
parameterlessStatement statement decoder value =
  fmap (either (Left . Text.decodeUtf8) id) $
  DB.session $ do
    integerDatetimes <- DB.integerDatetimes
    bytes <- DB.oneRow statement [] LibPQ.Binary
    return $ Decoder.run (decoder integerDatetimes) bytes

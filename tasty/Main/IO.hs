-- |
-- Specific IO Actions
module Main.IO where

import Main.Prelude hiding (assert, isRight, isLeft)
import Test.QuickCheck
import Test.QuickCheck.Instances
import qualified Main.PTI as PTI
import qualified Main.DB as DB
import qualified Data.Scientific as Scientific
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import qualified Data.Text.Encoding as Text
import qualified PostgreSQL.Binary.Data as Data
import qualified PostgreSQL.Binary.Encoder as Encoder
import qualified PostgreSQL.Binary.Decoder as Decoder


roundtrip :: DB.Oid -> (Bool -> Encoder.Encoder a) -> (Bool -> Decoder.Decoder a) -> a -> IO (Either Text a)
roundtrip oid encoder decoder value =
  fmap (either (Left . Text.decodeUtf8) id) $
  DB.session $ do
    integerDatetimes <- DB.integerDatetimes
    bytes <- DB.oneRow "SELECT $1" (params integerDatetimes) DB.Binary
    return $ Decoder.run (decoder integerDatetimes) bytes
  where
    params integerDatetimes =
      [ Just ( oid , bytes , DB.Binary ) ]
      where
        bytes =
          (convert . encoder integerDatetimes) value

parameterlessStatement :: ByteString -> (Bool -> Decoder.Decoder a) -> a -> IO (Either Text a)
parameterlessStatement statement decoder value =
  fmap (either (Left . Text.decodeUtf8) id) $
  DB.session $ do
    integerDatetimes <- DB.integerDatetimes
    bytes <- DB.oneRow statement [] DB.Binary
    return $ Decoder.run (decoder integerDatetimes) bytes

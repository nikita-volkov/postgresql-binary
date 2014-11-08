module PostgreSQLBinary.Encoder.Builder where

import PostgreSQLBinary.Prelude hiding (bool)
import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector as Vector
import qualified Data.ByteString.Builder.Scientific as Scientific
import qualified PostgreSQLBinary.Array as Array


{-# INLINE run #-}
run :: Builder -> B.ByteString
run = 
  BL.toStrict . toLazyByteString

bool :: Bool -> Builder
bool = 
  \case True -> word8 1; False -> word8 0

array :: Array.Data -> Builder
array (dimensionsV, valuesV, nullsV, oidV) =
  dimensionsLength <> nulls <> oid <> dimensions <> values
  where
    dimensionsLength = 
      word32BE $ fromIntegral $ length dimensionsV
    nulls = 
      word32BE $ if nullsV then 1 else 0
    oid = 
      word32BE oidV
    dimensions = 
      foldMap dimension dimensionsV
    values = 
      foldMap value valuesV
    dimension (w, l) = 
      word32BE w <> word32BE l
    value =
      \case
        Nothing -> word32BE (-1)
        Just b -> word32BE (fromIntegral (B.length b)) <> byteString b

time :: TimeOfDay -> Builder
time =
  word64BE . (`div` (10^6)) . unsafeCoerce timeOfDayToTime

timetz :: (TimeOfDay, TimeZone) -> Builder
timetz (timeX, tzX) =
  time timeX <> tz tzX

tz :: TimeZone -> Builder
tz =
  int32BE . fromIntegral . (*60) . negate . timeZoneMinutes

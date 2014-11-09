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
import qualified Data.Scientific as Scientific
import qualified Data.UUID as UUID
import qualified PostgreSQLBinary.Array as Array
import qualified PostgreSQLBinary.Date as Date
import qualified PostgreSQLBinary.Numeric as Numeric


{-# INLINE run #-}
run :: Builder -> B.ByteString
run = 
  BL.toStrict . toLazyByteString

{-# INLINE bool #-}
bool :: Bool -> Builder
bool = 
  \case True -> word8 1; False -> word8 0

{-# INLINE array #-}
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

{-# INLINE date #-}
date :: Day -> Builder
date =
  int32BE . fromIntegral . Date.dayToPostgresJulian

{-# INLINE time #-}
time :: TimeOfDay -> Builder
time =
  word64BE . (`div` (10^6)) . unsafeCoerce timeOfDayToTime

{-# INLINE timetz #-}
timetz :: (TimeOfDay, TimeZone) -> Builder
timetz (timeX, tzX) =
  time timeX <> tz tzX

{-# INLINE tz #-}
tz :: TimeZone -> Builder
tz =
  int32BE . fromIntegral . (*60) . negate . timeZoneMinutes

{-# INLINE timestamp #-}
timestamp :: UTCTime -> Builder
timestamp (UTCTime dayX timeX) =
  let days = Date.dayToPostgresJulian dayX * 10^6 * 60 * 60 * 24
      time = (`div` (10^6)) . unsafeCoerce $ timeX
      in int64BE $ fromIntegral $ days + time

{-# INLINE timestamptz #-}
timestamptz :: LocalTime -> Builder
timestamptz (LocalTime dayX timeX) =
  let days = Date.dayToPostgresJulian dayX * 10^6 * 60 * 60 * 24
      time = (`div` (10^6)) . unsafeCoerce timeOfDayToTime $ timeX
      in int64BE $ fromIntegral $ days + time

{-# INLINE interval #-}
interval :: DiffTime -> Builder
interval x =
  flip evalState (unsafeCoerce x :: Integer) $ do
    u <- state (`divMod` (10 ^ 6))
    d <- state (`divMod` (10 ^ 6 * 60 * 60 * 24))
    m <- get
    return $
      int64BE (fromIntegral u) <> int32BE (fromIntegral d) <> int32BE (fromIntegral m)

{-# INLINE numeric #-}
numeric :: Scientific -> Builder
numeric x =
  word16BE (fromIntegral componentsAmount) <>
  int16BE (fromIntegral pointIndex) <>
  word16BE signCode <>
  word16BE (fromIntegral trimmedExponent) <>
  foldMap word16BE components
  where
    componentsAmount = 
      length components
    coefficient =
      Scientific.coefficient x
    exponent = 
      Scientific.base10Exponent x
    components = 
      Numeric.extractComponents tunedCoefficient
    pointIndex =
      componentsAmount + (tunedExponent `div` 4) - 1
    (tunedCoefficient, tunedExponent) =
      case mod exponent 4 of
        0 -> (coefficient, exponent)
        x -> (coefficient * 10 ^ x, exponent - x)
    trimmedExponent =
      if tunedExponent >= 0
        then 0
        else negate tunedExponent
    signCode =
      if coefficient < 0
        then Numeric.negSignCode
        else Numeric.posSignCode

{-# INLINE uuid #-}
uuid :: UUID -> Builder
uuid =
  UUID.toWords >>> \(a, b, c, d) -> 
    word32BE a <> 
    word32BE b <> 
    word32BE c <> 
    word32BE d


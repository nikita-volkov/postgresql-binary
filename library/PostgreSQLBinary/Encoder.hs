module PostgreSQLBinary.Encoder where

import PostgreSQLBinary.Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import qualified PostgreSQLBinary.Encoder.Builder as Builder
import qualified PostgreSQLBinary.Array as Array
import qualified PostgreSQLBinary.Time as Time
import qualified PostgreSQLBinary.Integral as Integral
import qualified PostgreSQLBinary.Interval as Interval
import qualified PostgreSQLBinary.Numeric as Numeric
import qualified PostgreSQLBinary.Composite as Composite

-- |
-- A function for rendering a value into a byte string.
type E a = a -> ByteString


-- * Numbers
-------------------------

{-# INLINABLE int2 #-}
int2 :: E (Either Int16 Word16)
int2 = 
  either unpack unpack
  where
    unpack = Integral.unpackBySize 2

{-# INLINABLE int4 #-}
int4 :: E (Either Int32 Word32)
int4 = 
  either unpack unpack
  where
    unpack = Integral.unpackBySize 4

{-# INLINABLE int8 #-}
int8 :: E (Either Int64 Word64)
int8 = 
  either unpack unpack
  where
    unpack = Integral.unpackBySize 8

{-# INLINABLE float4 #-}
float4 :: E Float
float4 =
  int4 . Right . (unsafeCoerce :: Float -> Word32)

{-# INLINABLE float8 #-}
float8 :: E Double
float8 =
  int8 . Right . (unsafeCoerce :: Double -> Word64)

{-# INLINABLE numeric #-}
numeric :: E Scientific
numeric =
  Builder.run . Builder.numeric

-- * Text
-------------------------

-- |
-- A UTF-8-encoded char.
-- 
-- Note that since it's UTF-8-encoded
-- not a \"char\" but a \"text\" OID should be used with it.
{-# INLINABLE char #-}
char :: E Char
char = 
  text . Left . T.singleton

-- |
-- Either a strict or a lazy text.
{-# INLINABLE text #-}
text :: E (Either Text TL.Text)
text =
  either strict lazy
  where
    strict = TE.encodeUtf8 . T.filter (/= '\0')
    lazy = BL.toStrict . TLE.encodeUtf8 . TL.filter (/= '\0')

-- |
-- Either a strict or a lazy bytestring.
{-# INLINABLE bytea #-}
bytea :: E (Either ByteString BL.ByteString)
bytea =
  either id BL.toStrict

-- * Date and Time
-------------------------

{-# INLINABLE date #-}
date :: E Day
date =
  Builder.run . Builder.date

-- |
-- Encoding strategy depends on whether the server supports @integer_datetimes@.
{-# INLINABLE time #-}
time :: Bool -> E TimeOfDay
time integer_datetimes (TimeOfDay h m s) =
  let
    p = unsafeCoerce s :: Integer
    u = p `div` (10^6)
    in if integer_datetimes
      then
        Integral.unpackBySize 8 $
          fromIntegral u + 10^6 * 60 * (m + 60 * h)
      else
        inline float8 $
          fromIntegral u / 10^6 + 60 * (fromIntegral m + 60 * (fromIntegral h))

-- |
-- Encoding strategy depends on whether the server supports @integer_datetimes@.
{-# INLINABLE timetz #-}
timetz :: Bool -> E (TimeOfDay, TimeZone)
timetz integer_datetimes (timeX, tzX) =
  inline time integer_datetimes timeX <> tz tzX
  where
    tz =
      Integral.unpackBySize 4 . (*60) . negate . timeZoneMinutes

{-# INLINABLE timestamp #-}
timestamp :: Bool -> E LocalTime
timestamp =
  \case
    True -> int8 . Left . Time.localTimeToMicros
    False -> float8 . Time.localTimeToSecs

{-# INLINABLE timestamptz #-}
timestamptz :: Bool -> E UTCTime
timestamptz =
  \case
    True -> int8 . Left . Time.utcToMicros
    False -> float8 . Time.utcToSecs

{-# INLINABLE interval #-}
interval :: Bool -> E DiffTime
interval integerDatetimes x =
  Builder.run $ 
    (if integerDatetimes then BB.int64BE u else BB.doubleBE s) <>
    BB.int32BE d <>
    BB.int32BE m
  where
    Interval.Interval u d m = 
      fromMaybe (error "Too large DiffTime value for an interval") $
      Interval.fromDiffTime x
    s = fromIntegral u / (10^6)

-- * Misc
-------------------------

{-# INLINABLE bool #-}
bool :: E Bool
bool =
  \case
    False -> B.singleton 0
    True  -> B.singleton 1

{-# INLINABLE uuid #-}
uuid :: E UUID
uuid =
  Builder.run . Builder.uuid

{-# INLINABLE array #-}
array :: E Array.Data
array = 
  Builder.run . Builder.array

-- * Composite types
-------------------------

composite :: E (V.Vector Composite.Field)
composite vect = Builder.run (sizeTag <> foldMap field vect)
 where
  field :: Composite.Field -> BB.Builder
  field f = case f of
    Composite.NULL  oid         -> BB.int32BE oid <> BB.int32BE (-1)
    Composite.Field oid size bs -> BB.int32BE oid <> BB.int32BE size <>
                                   BB.byteString bs
  sizeTag :: BB.Builder
  sizeTag = BB.int32BE (fromIntegral (V.length vect) :: Int32)
    

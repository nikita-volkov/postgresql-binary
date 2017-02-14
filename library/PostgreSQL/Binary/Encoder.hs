{-# LANGUAGE CPP #-}
module PostgreSQL.Binary.Encoder
(
  run,
  -- * Value encoder
  Encoder,
  int2_int16,
  int2_word16,
  int4_int32,
  int4_word32,
  int8_int64,
  int8_word64,
  float4,
  float8,
  composite,
  bool,
  numeric,
  uuid,
  inet,
  json_ast,
  json_bytes,
  jsonb_ast,
  jsonb_bytes,
  char,
  text_strict,
  text_lazy,
  bytea_strict,
  bytea_lazy,
  date,
  time_int,
  time_float,
  timetz_int,
  timetz_float,
  timestamp_int,
  timestamp_float,
  timestamptz_int,
  timestamptz_float,
  interval_int,
  interval_float,
  hstore,
  hstoreRep,
  array,
  -- * Array encoder
  ArrayEncoder,
  arrayValue,
  arrayNullableValue,
  arrayDimension,
  arrayRep,
  -- * Enum
  enum,
)
where

import PostgreSQL.Binary.Prelude hiding (take, bool, maybe)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Vector as Vector
import qualified Data.Scientific as Scientific
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import qualified PostgreSQL.Binary.Data as Data
import qualified PostgreSQL.Binary.Integral as Integral
import qualified PostgreSQL.Binary.Numeric as Numeric
import qualified PostgreSQL.Binary.Time as Time
import qualified PostgreSQL.Binary.Interval as Interval
import qualified PostgreSQL.Binary.BuilderPrim as BuilderPrim
import qualified Control.Foldl as Foldl


type Encoder a =
  a -> Builder

{-# INLINE run #-}
run :: Encoder a -> a -> ByteString
run encoder =
  LazyByteString.toStrict . Builder.toLazyByteString . encoder

{-# INLINE tuple2 #-}
tuple2 :: Encoder a -> Encoder b -> Encoder (a, b)
tuple2 e1 e2 =
  \(v1, v2) -> e1 v1 <> e2 v2

{-# INLINE tuple3 #-}
tuple3 :: Encoder a -> Encoder b -> Encoder c -> Encoder (a, b, c)
tuple3 e1 e2 e3 =
  \(v1, v2, v3) -> e1 v1 <> e2 v2 <> e3 v3

{-# INLINE tuple4 #-}
tuple4 :: Encoder a -> Encoder b -> Encoder c -> Encoder d -> Encoder (a, b, c, d)
tuple4 e1 e2 e3 e4 =
  \(v1, v2, v3, v4) -> e1 v1 <> e2 v2 <> e3 v3 <> e4 v4

{-# INLINE tuple5 #-}
tuple5 :: Encoder a -> Encoder b -> Encoder c -> Encoder d -> Encoder e -> Encoder (a, b, c, d, e)
tuple5 e1 e2 e3 e4 e5 (v1, v2, v3, v4, v5) = e1 v1 <> e2 v2 <> e3 v3 <> e4 v4 <> e5 v5

{-# INLINE tuple8 #-}
tuple8 :: Encoder a -> Encoder b -> Encoder c -> Encoder d -> Encoder e -> Encoder f -> Encoder g -> Encoder h -> Encoder (a, b, c, d, e, f, g, h)
tuple8 e1 e2 e3 e4 e5 e6 e7 e8 (v1, v2, v3, v4, v5, v6, v7, v8) = e1 v1 <> e2 v2 <> e3 v3 <> e4 v4 <> e5 v5 <> e6 v6 <> e7 v7 <> e8 v8

{-# INLINE premap #-}
premap :: (a -> b) -> Encoder b -> Encoder a
premap f e =
  e . f

{-# INLINE int_int8 #-}
int_int8 :: Encoder Int8
int_int8 =
  Builder.int8

{-# INLINE int_word8 #-}
int_word8 :: Encoder Word8
int_word8 =
  Builder.word8

{-# INLINE int2_int16 #-}
int2_int16 :: Encoder Int16
int2_int16 =
  Builder.int16BE

{-# INLINE int2_word16 #-}
int2_word16 :: Encoder Word16
int2_word16 =
  Builder.word16BE

{-# INLINE int4_int32 #-}
int4_int32 :: Encoder Int32
int4_int32 =
  Builder.int32BE

{-# INLINE int4_word32 #-}
int4_word32 :: Encoder Word32
int4_word32 =
  Builder.word32BE

{-# INLINE int4_int #-}
int4_int :: Encoder Int
int4_int =
  int4_int32 . fromIntegral

{-# INLINE int8_int64 #-}
int8_int64 :: Encoder Int64
int8_int64 =
  Builder.int64BE

{-# INLINE int8_word64 #-}
int8_word64 :: Encoder Word64
int8_word64 =
  Builder.word64BE

{-# INLINE float4 #-}
float4 :: Encoder Float
float4 =
  int4_int32 . unsafeCoerce

{-# INLINE float8 #-}
float8 :: Encoder Double
float8 =
  int8_int64 . unsafeCoerce

{-# INLINE null4 #-}
null4 :: ByteStringBuilder
null4 =
  Builder.string7 "\255\255\255\255"

{-# INLINABLE composite #-}
composite :: Encoder Data.Composite
composite vector =
  int4_int (Vector.length vector) <>
  foldMap component vector
  where
    component (oid, theContent) =
      int4_word32 oid <> content theContent

{-# INLINABLE content #-}
content :: Encoder Data.Content
content =
  \case
    Nothing ->
      null4
    Just content ->
      int4_int (ByteString.length content) <>
      Builder.byteString content

{-# INLINABLE maybe #-}
maybe :: Encoder a -> Encoder (Maybe a)
maybe encoder =
  \case
    Nothing ->
      null4
    Just value ->
      run encoder value & \bytes -> int4_int (ByteString.length bytes) <> Builder.byteString bytes

{-# INLINE bool #-}
bool :: Encoder Bool
bool =
  \case
    True -> Builder.word8 1
    False -> Builder.word8 0

{-# INLINABLE numeric #-}
numeric :: Encoder Scientific
numeric x =
  int2_int16 (fromIntegral componentsAmount) <>
  int2_int16 (fromIntegral pointIndex) <>
  int2_word16 signCode <>
  int2_int16 (fromIntegral trimmedExponent) <>
  foldMap int2_int16 components
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

{-# INLINABLE uuid #-}
uuid :: Encoder UUID
uuid =
  premap UUID.toWords (tuple4 int4_word32 int4_word32 int4_word32 int4_word32)

{-# INLINABLE inetIPv4 #-}
inetIPv4 :: Encoder (Word8, Data.Netmask, Word8, Int8, Data.IPv4)
inetIPv4 =
  tuple5 int_word8 int_word8 int_word8 int_int8 $ tuple4 int_word8 int_word8 int_word8 int_word8

{-# INLINABLE inetIPv6 #-}
inetIPv6 :: Encoder (Word8, Data.Netmask, Word8, Int8, Data.IPv6)
inetIPv6 =
  tuple5 int_word8 int_word8 int_word8 int_int8 $
  tuple8 int2_word16 int2_word16 int2_word16 int2_word16 int2_word16 int2_word16 int2_word16 int2_word16

{-# INLINABLE inet #-}
inet :: Encoder Data.Inet
inet i@(Data.InetIPv4 ipv4) =
  premap (const (Data.afInet, Data.maxNetmaskIPv4, Data.isCidr, Data.ipv4Size, ipv4)) inetIPv4 i
inet i@(Data.InetIPv4Subnet ipv4 netmask) =
  premap (const (Data.afInet, netmask, Data.isCidr, Data.ipv4Size, ipv4)) inetIPv4 i
inet i@(Data.InetIPv6 ipv6) =
  premap (const (Data.afInet6, Data.maxNetmaskIPv6, Data.isCidr, Data.ipv6Size, ipv6)) inetIPv6 i
inet i@(Data.InetIPv6Subnet ipv6 netmask) =
  premap (const (Data.afInet6, netmask, Data.isCidr, Data.ipv6Size, ipv6)) inetIPv6 i

{-# INLINABLE json_ast #-}
json_ast :: Encoder Aeson.Value
#if MIN_VERSION_aeson(0,10,0)
json_ast =
  Aeson.fromEncoding . Aeson.toEncoding
#else
json_ast =
  Builder.lazyByteString . Aeson.encode
#endif

{-# INLINABLE json_bytes #-}
json_bytes :: Encoder ByteString
json_bytes =
  Builder.byteString

{-# INLINABLE jsonb_ast #-}
jsonb_ast :: Encoder Aeson.Value
jsonb_ast =
  \x -> "\1" <> json_ast x

{-# INLINABLE jsonb_bytes #-}
jsonb_bytes :: Encoder ByteString
jsonb_bytes =
  \x -> "\1" <> Builder.byteString x

-- * Text
-------------------------

-- |
-- A UTF-8-encoded char.
-- 
-- Note that since it's UTF-8-encoded
-- not the \"char\" but the \"text\" OID should be used with it.
{-# INLINABLE char #-}
char :: Encoder Char
char = 
  Builder.charUtf8

{-# INLINABLE text_strict #-}
text_strict :: Encoder Text
text_strict =
  Text.encodeUtf8BuilderEscaped BuilderPrim.nullByteIgnoringBoundedPrim

{-# INLINABLE text_lazy #-}
text_lazy :: Encoder LazyText.Text
text_lazy =
  LazyText.encodeUtf8BuilderEscaped BuilderPrim.nullByteIgnoringBoundedPrim

{-# INLINABLE bytea_strict #-}
bytea_strict :: Encoder ByteString
bytea_strict =
  Builder.byteString
  
{-# INLINABLE bytea_lazy #-}
bytea_lazy :: Encoder LazyByteString.ByteString
bytea_lazy =
  Builder.lazyByteString

-- * Date and Time
-------------------------

{-# INLINABLE date #-}
date :: Encoder Day
date =
  int4_int32 . fromIntegral . Time.dayToPostgresJulian

{-# INLINABLE time_int #-}
time_int :: Encoder TimeOfDay
time_int (TimeOfDay h m s) =
  let
    p = unsafeCoerce s :: Integer
    u = p `div` (10^6)
    in int8_int64 (fromIntegral u + 10^6 * 60 * (fromIntegral m + 60 * fromIntegral h))

{-# INLINABLE time_float #-}
time_float :: Encoder TimeOfDay
time_float (TimeOfDay h m s) =
  let
    p = unsafeCoerce s :: Integer
    u = p `div` (10^6)
    in float8 (fromIntegral u / 10^6 + 60 * (fromIntegral m + 60 * (fromIntegral h)))

{-# INLINABLE timetz_int #-}
timetz_int :: Encoder (TimeOfDay, TimeZone)
timetz_int (timeX, tzX) =
  time_int timeX <> tz tzX

{-# INLINABLE timetz_float #-}
timetz_float :: Encoder (TimeOfDay, TimeZone)
timetz_float (timeX, tzX) =
  time_float timeX <> tz tzX

{-# INLINE tz #-}
tz :: Encoder TimeZone
tz =
  int4_int . (*60) . negate . timeZoneMinutes

{-# INLINABLE timestamp_int #-}
timestamp_int :: Encoder LocalTime
timestamp_int =
  int8_int64 . Time.localTimeToMicros

{-# INLINABLE timestamp_float #-}
timestamp_float :: Encoder LocalTime
timestamp_float =
  float8 . Time.localTimeToSecs

{-# INLINABLE timestamptz_int #-}
timestamptz_int :: Encoder UTCTime
timestamptz_int =
  int8_int64 . Time.utcToMicros

{-# INLINABLE timestamptz_float #-}
timestamptz_float :: Encoder UTCTime
timestamptz_float =
  float8 . Time.utcToSecs

{-# INLINABLE interval_int #-}
interval_int :: Encoder DiffTime
interval_int x =
    Builder.int64BE u <>
    Builder.int32BE d <>
    Builder.int32BE m
  where
    Interval.Interval u d m = 
      fromMaybe (error ("Too large DiffTime value for an interval " <> show x)) $
      Interval.fromDiffTime x

{-# INLINABLE interval_float #-}
interval_float :: Encoder DiffTime
interval_float x =
    Builder.doubleBE s <>
    Builder.int32BE d <>
    Builder.int32BE m
  where
    Interval.Interval u d m = 
      fromMaybe (error ("Too large DiffTime value for an interval " <> show x)) $
      Interval.fromDiffTime x
    s =
      fromIntegral u / (10^6)


-- * Array
-------------------------

newtype ArrayEncoder a =
  ArrayEncoder (a -> (Builder, [Int32], Bool))

{-# INLINABLE array #-}
array :: Word32 -> ArrayEncoder a -> Encoder a
array oid (ArrayEncoder encoder) =
  \value ->
    let
      (valuesBuilder, dimensions, nulls) =
        encoder value
      (dimensionsAmount, dimensionsBuilder) =
        let
          step (amount, builder) dimension =
            (succ amount, builder <> Builder.int32BE dimension <> Builder.word32BE 1)
          init =
            (0, mempty)
          in
            foldl' step init dimensions
      nullsBuilder =
        Builder.word32BE (if nulls then 1 else 0)
      in
        Builder.word32BE dimensionsAmount <> nullsBuilder <> Builder.word32BE oid <> dimensionsBuilder <> valuesBuilder

{-# INLINABLE arrayValue #-}
arrayValue :: Encoder a -> ArrayEncoder a
arrayValue encoder =
  ArrayEncoder $ \value ->
    let
      bytes =
        run encoder value
      builder =
        Builder.word32BE (fromIntegral (ByteString.length bytes)) <>
        Builder.byteString bytes
      in
        (builder, [], False)

{-# INLINABLE arrayNullableValue #-}
arrayNullableValue :: Encoder a -> ArrayEncoder (Maybe a)
arrayNullableValue encoder =
  ArrayEncoder $ \case
    Nothing ->
      (int4_int32 (-1), [], True)
    Just value ->
      let
        bytes =
          run encoder value
        builder =
          Builder.word32BE (fromIntegral (ByteString.length bytes)) <>
          Builder.byteString bytes
        in
          (builder, [], False)

{-# INLINABLE arrayDimension #-}
arrayDimension :: (forall a. (a -> b -> a) -> a -> c -> a) -> ArrayEncoder b -> ArrayEncoder c
arrayDimension foldl (ArrayEncoder encoder) =
  ArrayEncoder $ \value ->
    let
      step (builder, _, length, nulls) value =
        let
          (valueBuilder, valueDimensions, valueNulls) = encoder value
          in
            (builder <> valueBuilder, valueDimensions, succ length, nulls || valueNulls)
      init =
        (mempty, [], 0, False)
      (foldedBuilder, foldedDimensions, foldedLength, foldedNulls) =
        foldl step init value
      resultDimensions =
        foldedLength : foldedDimensions
      in
        (foldedBuilder, resultDimensions, foldedNulls)
        

-- * Array rep
-------------------------

{-# INLINABLE arrayRep #-}
arrayRep :: Encoder Data.Array
arrayRep (dimensionsV, valuesV, nullsV, oidV) =
  dimensionsLength <> nulls <> oid <> dimensions <> values
  where
    dimensionsLength = 
      int4_word32 $ fromIntegral $ Vector.length dimensionsV
    nulls = 
      int4_word32 $ if nullsV then 1 else 0
    oid = 
      int4_word32 oidV
    dimensions = 
      foldMap dimension dimensionsV
    values = 
      foldMap value valuesV
    dimension (w, l) = 
      int4_word32 w <> int4_word32 l
    value =
      \case
        Nothing -> int4_int32 (-1)
        Just b -> int4_int32 (fromIntegral (ByteString.length b)) <> Builder.byteString b


-- * HStore
-------------------------

-- |
-- A polymorphic in-place @HSTORE@ encoder.
-- 
-- Accepts:
-- 
-- * An implementation of the @foldl@ function
-- (e.g., @Data.Foldable.'foldl''@),
-- which determines the input value.
-- 
-- Here's how you can use it to produce a specific encoder:
-- 
-- @
-- hashMapHStore :: Encoder (Data.HashMap.Strict.HashMap Text (Maybe Text))
-- hashMapHStore =
--   hstore foldl'
-- @
-- 
{-# INLINABLE hstore #-}
hstore :: (forall a. (a -> (Text, Maybe Text) -> a) -> a -> b -> a) -> Encoder b
hstore foldl =
  fold & \(Foldl.Fold step init fin) -> fin . foldl step init
  where
    fold =
      (<>) <$> componentsAmount <*> components
      where
        componentsAmount =
          fmap int4_int Foldl.length
        components =
          Foldl.foldMap componentBuilder id
          where
            componentBuilder (key, value) =
              text_strict key <> maybe text_strict value

{-# INLINABLE hstoreRep #-}
hstoreRep :: Encoder Data.HStore
hstoreRep vector =
  int4_int32 (fromIntegral (Vector.length vector)) <>
  foldMap component vector
  where
    component (key, value) =
      Builder.byteString key <> content value


-- * Enum
-------------------------

-- |
-- Given a function,
-- which maps the value into the textual enum label from the DB side,
-- produces an encoder of that value
-- 
{-# INLINE enum #-}
enum :: (a -> Text) -> Encoder a
enum asText =
  text_strict . asText

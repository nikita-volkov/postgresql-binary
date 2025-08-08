module PostgreSQL.Binary.Encoding.Builders where

import ByteString.StrictBuilder
import qualified Data.Aeson as R
import qualified Data.ByteString.Builder as M
import qualified Data.ByteString.Lazy as N
import qualified Data.HashMap.Strict as F
import qualified Data.IP as G
import qualified Data.Map.Strict as Q
import qualified Data.Scientific as D
import qualified Data.Text.Encoding as J
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as K
import qualified Data.UUID as E
import qualified Data.Vector as A
import qualified PostgreSQL.Binary.BuilderPrim as I
import qualified PostgreSQL.Binary.Inet as H
import qualified PostgreSQL.Binary.Interval as P
import qualified PostgreSQL.Binary.Numeric as C
import PostgreSQL.Binary.Prelude hiding (bool)
import qualified PostgreSQL.Binary.Prelude as B
import qualified PostgreSQL.Binary.Range as S
import qualified PostgreSQL.Binary.Time as O

-- * Helpers

{-# NOINLINE null4 #-}
null4 :: Builder
null4 =
  int4_int (-1)

{-# INLINE sized #-}
sized :: Builder -> Builder
sized payload =
  int4_int (builderLength payload)
    <> payload

{-# INLINE sizedMaybe #-}
sizedMaybe :: (element -> Builder) -> Maybe element -> Builder
sizedMaybe elementBuilder =
  B.maybe null4 (sized . elementBuilder)

{-# NOINLINE true1 #-}
true1 :: Builder
true1 =
  word8 1

{-# NOINLINE false1 #-}
false1 :: Builder
false1 =
  word8 0

{-# NOINLINE true4 #-}
true4 :: Builder
true4 =
  int4_word32 1

{-# NOINLINE false4 #-}
false4 :: Builder
false4 =
  int4_word32 0

-- * Primitives

{-# INLINE bool #-}
bool :: Bool -> Builder
bool =
  B.bool false1 true1

{-# INLINE int2_int16 #-}
int2_int16 :: Int16 -> Builder
int2_int16 =
  int16BE

{-# INLINE int2_word16 #-}
int2_word16 :: Word16 -> Builder
int2_word16 =
  word16BE

{-# INLINE int4_int32 #-}
int4_int32 :: Int32 -> Builder
int4_int32 =
  int32BE

{-# INLINE int4_word32 #-}
int4_word32 :: Word32 -> Builder
int4_word32 =
  word32BE

{-# INLINE int4_int #-}
int4_int :: Int -> Builder
int4_int =
  int4_int32 . fromIntegral

{-# INLINE int8_int64 #-}
int8_int64 :: Int64 -> Builder
int8_int64 =
  int64BE

{-# INLINE int8_word64 #-}
int8_word64 :: Word64 -> Builder
int8_word64 =
  word64BE

{-# INLINE float4 #-}
float4 :: Float -> Builder
float4 =
  int4_int32 . unsafeCoerce

{-# INLINE float8 #-}
float8 :: Double -> Builder
float8 =
  int8_int64 . unsafeCoerce

{-# INLINEABLE numeric #-}
numeric :: Scientific -> Builder
numeric x =
  word16BE (fromIntegral componentsAmount)
    <> word16BE (fromIntegral pointIndex)
    <> signCode
    <> word16BE (fromIntegral trimmedExponent)
    <> foldMap word16BE components
  where
    componentsAmount =
      length components
    coefficient =
      D.coefficient x
    exponent =
      D.base10Exponent x
    components =
      C.extractComponents tunedCoefficient
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
        then numericNegSignCode
        else numericPosSignCode

{-# NOINLINE numericNegSignCode #-}
numericNegSignCode :: Builder
numericNegSignCode =
  int2_word16 C.negSignCode

{-# NOINLINE numericPosSignCode #-}
numericPosSignCode :: Builder
numericPosSignCode =
  int2_word16 C.posSignCode

{-# INLINE uuid #-}
uuid :: UUID -> Builder
uuid uuid =
  case E.toWords uuid of
    (w1, w2, w3, w4) -> int4_word32 w1 <> int4_word32 w2 <> int4_word32 w3 <> int4_word32 w4

{-# INLINEABLE ip4 #-}
ip4 :: G.IPv4 -> Builder
ip4 =
  int4_word32 . G.fromIPv4w

{-# INLINEABLE ip6 #-}
ip6 :: G.IPv6 -> Builder
ip6 x =
  case G.fromIPv6w x of
    (w1, w2, w3, w4) -> int4_word32 w1 <> int4_word32 w2 <> int4_word32 w3 <> int4_word32 w4

{-# INLINEABLE ip4range #-}
ip4range :: G.AddrRange G.IPv4 -> Builder
ip4range x =
  case G.addrRangePair x of
    (addr, mlen) -> inetAddressFamily <> netLength mlen <> isCidr <> ip4Size <> ip4 addr
  where
    netLength =
      word8 . fromIntegral
    isCidr =
      false1

{-# INLINEABLE ip6range #-}
ip6range :: G.AddrRange G.IPv6 -> Builder
ip6range x =
  case G.addrRangePair x of
    (addr, mlen) -> inet6AddressFamily <> netLength mlen <> isCidr <> ip6Size <> ip6 addr
  where
    netLength =
      word8 . fromIntegral
    isCidr =
      false1

{-# INLINEABLE inet #-}
inet :: G.IPRange -> Builder
inet (G.IPv4Range x) = ip4range x
inet (G.IPv6Range x) = ip6range x

{-# NOINLINE inetAddressFamily #-}
inetAddressFamily :: Builder
inetAddressFamily =
  word8 H.inetAddressFamily

{-# NOINLINE inet6AddressFamily #-}
inet6AddressFamily :: Builder
inet6AddressFamily =
  word8 H.inet6AddressFamily

{-# NOINLINE ip4Size #-}
ip4Size :: Builder
ip4Size =
  word8 4

{-# NOINLINE ip6Size #-}
ip6Size :: Builder
ip6Size =
  word8 16

{-# INLINEABLE macaddr #-}
macaddr :: (Word8, Word8, Word8, Word8, Word8, Word8) -> Builder
macaddr (a, b, c, d, e, f) =
  word8 a <> word8 b <> word8 c <> word8 d <> word8 e <> word8 f

-- * Text

-- |
-- A UTF-8-encoded char.
--
-- Note that since it's UTF-8-encoded
-- not the \"char\" but the \"text\" OID should be used with it.
{-# INLINE char_utf8 #-}
char_utf8 :: Char -> Builder
char_utf8 =
  utf8Char

{-# INLINE text_strict #-}
text_strict :: Text -> Builder
text_strict =
  bytea_lazyBuilder . J.encodeUtf8BuilderEscaped I.nullByteIgnoringBoundedPrim

{-# INLINE text_lazy #-}
text_lazy :: L.Text -> Builder
text_lazy =
  bytea_lazyBuilder . K.encodeUtf8BuilderEscaped I.nullByteIgnoringBoundedPrim

{-# INLINE bytea_strict #-}
bytea_strict :: ByteString -> Builder
bytea_strict =
  bytes

{-# INLINE bytea_lazy #-}
bytea_lazy :: N.ByteString -> Builder
bytea_lazy =
  lazyBytes

{-# INLINE bytea_lazyBuilder #-}
bytea_lazyBuilder :: M.Builder -> Builder
bytea_lazyBuilder =
  lazyBytes . M.toLazyByteString

-- * Time

{-# INLINE date #-}
date :: Day -> Builder
date =
  int4_int32 . fromIntegral . O.dayToPostgresJulian

{-# INLINEABLE time_int #-}
time_int :: TimeOfDay -> Builder
time_int (TimeOfDay h m s) =
  let p = unsafeCoerce s :: Integer
      u = p `div` (10 ^ 6)
   in int8_int64 (fromIntegral u + 10 ^ 6 * 60 * (fromIntegral m + 60 * fromIntegral h))

{-# INLINEABLE time_float #-}
time_float :: TimeOfDay -> Builder
time_float (TimeOfDay h m s) =
  let p = unsafeCoerce s :: Integer
      u = p `div` (10 ^ 6)
   in float8 (fromIntegral u / 10 ^ 6 + 60 * (fromIntegral m + 60 * (fromIntegral h)))

{-# INLINE timetz_int #-}
timetz_int :: (TimeOfDay, TimeZone) -> Builder
timetz_int (timeX, tzX) =
  time_int timeX <> tz tzX

{-# INLINE timetz_float #-}
timetz_float :: (TimeOfDay, TimeZone) -> Builder
timetz_float (timeX, tzX) =
  time_float timeX <> tz tzX

{-# INLINE tz #-}
tz :: TimeZone -> Builder
tz =
  int4_int . (* 60) . negate . timeZoneMinutes

{-# INLINE timestamp_int #-}
timestamp_int :: LocalTime -> Builder
timestamp_int =
  int8_int64 . O.localTimeToMicros

{-# INLINE timestamp_float #-}
timestamp_float :: LocalTime -> Builder
timestamp_float =
  float8 . O.localTimeToSecs

{-# INLINE timestamptz_int #-}
timestamptz_int :: UTCTime -> Builder
timestamptz_int =
  int8_int64 . O.utcToMicros

{-# INLINE timestamptz_float #-}
timestamptz_float :: UTCTime -> Builder
timestamptz_float =
  float8 . O.utcToSecs

{-# INLINEABLE interval_int #-}
interval_int :: DiffTime -> Builder
interval_int x =
  int64BE u
    <> int32BE d
    <> int32BE m
  where
    P.Interval u d m =
      fromMaybe (error ("Too large DiffTime value for an interval " <> show x))
        $ P.fromDiffTime x

{-# INLINEABLE interval_float #-}
interval_float :: DiffTime -> Builder
interval_float x =
  float8 s
    <> int32BE d
    <> int32BE m
  where
    P.Interval u d m =
      fromMaybe (error ("Too large DiffTime value for an interval " <> show x))
        $ P.fromDiffTime x
    s =
      fromIntegral u / (10 ^ 6)

-- * JSON

{-# INLINE json_bytes #-}
json_bytes :: ByteString -> Builder
json_bytes =
  bytes

{-# INLINE json_bytes_lazy #-}
json_bytes_lazy :: N.ByteString -> Builder
json_bytes_lazy =
  lazyBytes

{-# INLINE json_ast #-}
json_ast :: R.Value -> Builder
json_ast =
  lazyBytes . R.encode

{-# INLINE jsonb_bytes #-}
jsonb_bytes :: ByteString -> Builder
jsonb_bytes =
  mappend "\1" . bytes

{-# INLINE jsonb_bytes_lazy #-}
jsonb_bytes_lazy :: N.ByteString -> Builder
jsonb_bytes_lazy =
  mappend "\1" . lazyBytes

{-# INLINE jsonb_ast #-}
jsonb_ast :: R.Value -> Builder
jsonb_ast =
  mappend "\1" . json_ast

-- * Array

{-# INLINE array_vector #-}
array_vector :: Word32 -> (element -> Builder) -> Vector element -> Builder
array_vector oid elementBuilder vector =
  array oid dimensions False payload
  where
    dimensions =
      [fromIntegral (A.length vector)]
    payload =
      foldMap (sized . elementBuilder) vector

{-# INLINE nullableArray_vector #-}
nullableArray_vector :: Word32 -> (element -> Builder) -> Vector (Maybe element) -> Builder
nullableArray_vector oid elementBuilder vector =
  array oid dimensions True payload
  where
    dimensions =
      [fromIntegral (A.length vector)]
    payload =
      foldMap (sizedMaybe elementBuilder) vector

{-# INLINEABLE array #-}
array :: Word32 -> [Int32] -> Bool -> Builder -> Builder
array oid dimensions nulls payload =
  int4_int (B.length dimensions)
    <> B.bool false4 true4 nulls
    <> int4_word32 oid
    <> foldMap arrayDimension dimensions
    <> payload

{-# INLINE arrayDimension #-}
arrayDimension :: Int32 -> Builder
arrayDimension dimension =
  int4_int32 dimension <> true4

-- * HStore

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
-- hashMapHStore :: Data.HashMap.Strict.HashMap Text (Maybe Text) -> Builder
-- hashMapHStore =
--   hStoreUsingFoldl foldl'
-- @
{-# INLINEABLE hStoreUsingFoldl #-}
hStoreUsingFoldl :: (forall a. (a -> (Text, Maybe Text) -> a) -> a -> b -> a) -> b -> Builder
hStoreUsingFoldl foldl =
  exit . foldl progress enter
  where
    enter =
      (0, mempty)
    progress (!count, !payload) (key, value) =
      (succ count, payload <> hStoreRow key value)
    exit (count, payload) =
      int4_int count <> payload

{-# INLINE hStoreUsingFoldMapAndSize #-}
hStoreUsingFoldMapAndSize :: (forall a. (Monoid a) => ((Text, Maybe Text) -> a) -> b -> a) -> Int -> b -> Builder
hStoreUsingFoldMapAndSize foldMap size input =
  int4_int size <> foldMap (uncurry hStoreRow) input

{-# INLINE hStoreFromFoldMapAndSize #-}
hStoreFromFoldMapAndSize :: (forall a. (Monoid a) => (Text -> Maybe Text -> a) -> a) -> Int -> Builder
hStoreFromFoldMapAndSize foldMap size =
  int4_int size <> foldMap hStoreRow

{-# INLINE hStoreRow #-}
hStoreRow :: Text -> Maybe Text -> Builder
hStoreRow key value =
  sized (text_strict key) <> sizedMaybe text_strict value

{-# INLINE hStore_hashMap #-}
hStore_hashMap :: HashMap Text (Maybe Text) -> Builder
hStore_hashMap input =
  int4_int (F.size input)
    <> F.foldlWithKey' (\payload key value -> payload <> hStoreRow key value) mempty input

{-# INLINE hStore_map #-}
hStore_map :: Map Text (Maybe Text) -> Builder
hStore_map input =
  int4_int (Q.size input)
    <> Q.foldlWithKey' (\payload key value -> payload <> hStoreRow key value) mempty input

{-# INLINE range #-}
range :: (a -> Builder) -> S.Range a -> Builder
range builder r =
  case r of
    S.Empty -> word8 0x01
    S.Range S.Inf S.Inf -> word8 0x18
    S.Range (S.Excl l) (S.Excl r) -> word8 0x00 <> sized (builder l) <> sized (builder r)
    S.Range (S.Incl l) (S.Excl r) -> word8 0x02 <> sized (builder l) <> sized (builder r)
    S.Range (S.Excl l) (S.Incl r) -> word8 0x04 <> sized (builder l) <> sized (builder r)
    S.Range (S.Incl l) (S.Incl r) -> word8 0x06 <> sized (builder l) <> sized (builder r)
    S.Range (S.Excl l) S.Inf -> word8 0x10 <> sized (builder l)
    S.Range (S.Incl l) S.Inf -> word8 0x12 <> sized (builder l)
    S.Range S.Inf (S.Excl r) -> word8 0x08 <> sized (builder r)
    S.Range S.Inf (S.Incl r) -> word8 0x0C <> sized (builder r)

{-# INLINE multirange #-}
multirange :: (a -> Builder) -> S.Multirange a -> Builder
multirange builder ranges =
  int4_int (fromIntegral (length ranges))
    <> foldMap (sized . range builder) ranges
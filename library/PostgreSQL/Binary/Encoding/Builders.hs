module PostgreSQL.Binary.Encoding.Builders
where

import PostgreSQL.Binary.Prelude hiding (bool)
import StrictBytesBuilder
import qualified Data.Vector as A
import qualified Data.Scientific as D
import qualified Data.UUID as E
import qualified Data.ByteString.Builder as M
import qualified Data.ByteString.Lazy as N
import qualified Data.Text.Encoding as J
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as K
import qualified Network.IP.Addr as G
import qualified PostgreSQL.Binary.Prelude as B
import qualified PostgreSQL.Binary.Numeric as C
import qualified PostgreSQL.Binary.Inet as H
import qualified PostgreSQL.Binary.BuilderPrim as I
import qualified PostgreSQL.Binary.Time as O
import qualified PostgreSQL.Binary.Interval as P


-- * Helpers
-------------------------

{-# NOINLINE null4 #-}
null4 :: Builder
null4 =
  int4FromInt (-1)

{-# INLINE sized #-}
sized :: Builder -> Builder
sized payload =
  int4FromInt (builderLength payload) <>
  payload

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
  int4FromWord32 1

{-# NOINLINE false4 #-}
false4 :: Builder
false4 =
  int4FromWord32 0


-- * Primitives
-------------------------

{-# INLINE bool #-}
bool :: Bool -> Builder
bool =
  B.bool false1 true1

{-# INLINE intFromInt8 #-}
intFromInt8 :: Int8 -> Builder
intFromInt8 =
  int8

{-# INLINE intFromWord8 #-}
intFromWord8 :: Word8 -> Builder
intFromWord8 =
  word8

{-# INLINE int2FromInt16 #-}
int2FromInt16 :: Int16 -> Builder
int2FromInt16 =
  int16BE

{-# INLINE int2FromWord16 #-}
int2FromWord16 :: Word16 -> Builder
int2FromWord16 =
  word16BE

{-# INLINE int4FromInt32 #-}
int4FromInt32 :: Int32 -> Builder
int4FromInt32 =
  int32BE

{-# INLINE int4FromWord32 #-}
int4FromWord32 :: Word32 -> Builder
int4FromWord32 =
  word32BE

{-# INLINE int4FromInt #-}
int4FromInt :: Int -> Builder
int4FromInt =
  int4FromInt32 . fromIntegral

{-# INLINE int8FromInt64 #-}
int8FromInt64 :: Int64 -> Builder
int8FromInt64 =
  int64BE

{-# INLINE int8FromWord64 #-}
int8FromWord64 :: Word64 -> Builder
int8FromWord64 =
  word64BE

{-# INLINE float4 #-}
float4 :: Float -> Builder
float4 =
  int4FromInt32 . unsafeCoerce

{-# INLINE float8 #-}
float8 :: Double -> Builder
float8 =
  int8FromInt64 . unsafeCoerce

{-# INLINABLE numeric #-}
numeric :: Scientific -> Builder
numeric x =
  int2FromInt16 (fromIntegral componentsAmount) <>
  int2FromInt16 (fromIntegral pointIndex) <>
  signCode <>
  int2FromInt16 (fromIntegral trimmedExponent) <>
  foldMap int2FromInt16 components
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
  int2FromWord16 C.negSignCode

{-# NOINLINE numericPosSignCode #-}
numericPosSignCode :: Builder
numericPosSignCode =
  int2FromWord16 C.posSignCode

{-# INLINE uuid #-}
uuid :: UUID -> Builder
uuid uuid =
  case E.toWords uuid of
    (w1, w2, w3, w4) -> int4FromWord32 w1 <> int4FromWord32 w2 <> int4FromWord32 w3 <> int4FromWord32 w4

{-# INLINABLE ip4 #-}
ip4 :: G.IP4 -> Builder
ip4 x =
  case G.ip4ToOctets x of
    (w1, w2, w3, w4) -> intFromWord8 w1 <> intFromWord8 w2 <> intFromWord8 w3 <> intFromWord8 w4

{-# INLINABLE ip6 #-}
ip6 :: G.IP6 -> Builder
ip6 x =
  case G.ip6ToWords x of
    (w1, w2, w3, w4, w5, w6, w7, w8) ->
      int2FromWord16 w1 <> int2FromWord16 w2 <> int2FromWord16 w3 <> int2FromWord16 w4 <>
      int2FromWord16 w5 <> int2FromWord16 w6 <> int2FromWord16 w7 <> int2FromWord16 w8

{-# INLINABLE inet #-}
inet :: G.NetAddr G.IP -> Builder
inet i =
  case G.netHost i of
    G.IPv4 x -> inetAddressFamily <> netLength <> isCidr <> ip4Size <> ip4 x
    G.IPv6 x -> inet6AddressFamily <> netLength <> isCidr <> ip6Size <> ip6 x
    where
      netLength =
        intFromWord8 (G.netLength i)
      isCidr =
        false1

{-# NOINLINE inetAddressFamily #-}
inetAddressFamily :: Builder
inetAddressFamily =
  intFromWord8 H.inetAddressFamily

{-# NOINLINE inet6AddressFamily #-}
inet6AddressFamily :: Builder
inet6AddressFamily =
  intFromWord8 H.inet6AddressFamily

{-# NOINLINE ip4Size #-}
ip4Size :: Builder
ip4Size =
  intFromInt8 4

{-# NOINLINE ip6Size #-}
ip6Size :: Builder
ip6Size =
  intFromInt8 16


-- * Text
-------------------------

-- |
-- A UTF-8-encoded char.
-- 
-- Note that since it's UTF-8-encoded
-- not the \"char\" but the \"text\" OID should be used with it.
{-# INLINE charInUTF8 #-}
charInUTF8 :: Char -> Builder
charInUTF8 = 
  utf8Char

{-# INLINE textFromStrict #-}
textFromStrict :: Text -> Builder
textFromStrict =
  byteaFromLazyBuilder . J.encodeUtf8BuilderEscaped I.nullByteIgnoringBoundedPrim

{-# INLINE textFromLazy #-}
textFromLazy :: L.Text -> Builder
textFromLazy =
  byteaFromLazyBuilder . K.encodeUtf8BuilderEscaped I.nullByteIgnoringBoundedPrim

{-# INLINE byteaFromStrict #-}
byteaFromStrict :: ByteString -> Builder
byteaFromStrict =
  bytes

{-# INLINE byteaFromLazy #-}
byteaFromLazy :: N.ByteString -> Builder
byteaFromLazy =
  lazyBytes

{-# INLINE byteaFromLazyBuilder #-}
byteaFromLazyBuilder :: M.Builder -> Builder
byteaFromLazyBuilder =
  lazyBytes . M.toLazyByteString


-- * Time
-------------------------

{-# INLINE date #-}
date :: Day -> Builder
date =
  int4FromInt32 . fromIntegral . O.dayToPostgresJulian

{-# INLINABLE intTime #-}
intTime :: TimeOfDay -> Builder
intTime (TimeOfDay h m s) =
  let
    p = unsafeCoerce s :: Integer
    u = p `div` (10^6)
    in int8FromInt64 (fromIntegral u + 10^6 * 60 * (fromIntegral m + 60 * fromIntegral h))

{-# INLINABLE floatTime #-}
floatTime :: TimeOfDay -> Builder
floatTime (TimeOfDay h m s) =
  let
    p = unsafeCoerce s :: Integer
    u = p `div` (10^6)
    in float8 (fromIntegral u / 10^6 + 60 * (fromIntegral m + 60 * (fromIntegral h)))

{-# INLINE intTimetz #-}
intTimetz :: (TimeOfDay, TimeZone) -> Builder
intTimetz (timeX, tzX) =
  intTime timeX <> tz tzX

{-# INLINE floatTimetz #-}
floatTimetz :: (TimeOfDay, TimeZone) -> Builder
floatTimetz (timeX, tzX) =
  floatTime timeX <> tz tzX

{-# INLINE tz #-}
tz :: TimeZone -> Builder
tz =
  int4FromInt . (*60) . negate . timeZoneMinutes

{-# INLINE intTimestamp #-}
intTimestamp :: LocalTime -> Builder
intTimestamp =
  int8FromInt64 . O.localTimeToMicros

{-# INLINE floatTimestamp #-}
floatTimestamp :: LocalTime -> Builder
floatTimestamp =
  float8 . O.localTimeToSecs

{-# INLINE intTimestamptz #-}
intTimestamptz :: UTCTime -> Builder
intTimestamptz =
  int8FromInt64 . O.utcToMicros

{-# INLINE floatTimestamptz #-}
floatTimestamptz :: UTCTime -> Builder
floatTimestamptz =
  float8 . O.utcToSecs

{-# INLINABLE intInterval #-}
intInterval :: DiffTime -> Builder
intInterval x =
  int64BE u <>
  int32BE d <>
  int32BE m
  where
    P.Interval u d m = 
      fromMaybe (error ("Too large DiffTime value for an interval " <> show x)) $
      P.fromDiffTime x

{-# INLINABLE floatInterval #-}
floatInterval :: DiffTime -> Builder
floatInterval x =
  float8 s <>
  int32BE d <>
  int32BE m
  where
    P.Interval u d m = 
      fromMaybe (error ("Too large DiffTime value for an interval " <> show x)) $
      P.fromDiffTime x
    s =
      fromIntegral u / (10^6)


-- * JSON
-------------------------

{-# INLINE jsonFromBytes #-}
jsonFromBytes :: ByteString -> Builder
jsonFromBytes =
  bytes

{-# INLINE jsonbFromBytes #-}
jsonbFromBytes :: ByteString -> Builder
jsonbFromBytes =
  mappend "\1" . bytes


-- * Array
-------------------------

{-# INLINE arrayFromVector #-}
arrayFromVector :: Word32 -> (element -> Builder) -> Vector element -> Builder
arrayFromVector oid elementBuilder vector =
  array oid dimensions False payload
  where
    dimensions =
      [fromIntegral (A.length vector)]
    payload =
      foldMap (sized . elementBuilder) vector

{-# INLINE arrayFromNullableVector #-}
arrayFromNullableVector :: Word32 -> (element -> Builder) -> Vector (Maybe element) -> Builder
arrayFromNullableVector oid elementBuilder vector =
  array oid dimensions True payload
  where
    dimensions =
      [fromIntegral (A.length vector)]
    payload =
      foldMap (sizedMaybe elementBuilder) vector

{-# INLINABLE array #-}
array :: Word32 -> [Int32] -> Bool -> Builder -> Builder
array oid dimensions nulls payload =
  int4FromInt (B.length dimensions) <>
  B.bool false4 true4 nulls <>
  int4FromWord32 oid <>
  foldMap arrayDimension dimensions <>
  payload

{-# INLINE arrayDimension #-}
arrayDimension :: Int32 -> Builder
arrayDimension dimension =
  int4FromInt32 dimension <> true4

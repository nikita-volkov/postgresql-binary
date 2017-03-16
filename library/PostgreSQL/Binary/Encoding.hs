module PostgreSQL.Binary.Encoding
(
  valueBytes,

  -- * Value
  Value,
  primitiveValue,
  arrayValue,
  array_foldable,
  array_vector,
  nullableArray_vector,
  hStore_foldable,
  hStore_hashMap,
  hStore_map,

  -- * Primitive
  Primitive,
  bool,
  int2_int16,
  int2_word16,
  int4_int32,
  int4_word32,
  int8_int64,
  int8_word64,
  float4,
  float8,
  numeric,
  uuid,
  inet,
  char_utf8,
  text_strict,
  text_lazy,
  bytea_strict,
  bytea_lazy,
  bytea_lazyBuilder,
  -- ** Time
  -- | Some of the functions in this section are distinguished based
  -- on the @integer_datetimes@ setting of the server.
  date,
  time_int,
  time_float,
  timetz_int,
  timetz_float,
  tz,
  timestamp_int,
  timestamp_float,
  timestamptz_int,
  timestamptz_float,
  interval_int,
  interval_float,
  -- ** JSON
  json_bytes,
  jsonb_bytes,

  -- * Array
  Array,
  primitiveArray,
  nullArray,
  dimensionArray,
)
where

import PostgreSQL.Binary.Prelude hiding (bool, length)
import qualified StrictBytesBuilder as C
import qualified Data.Vector as A
import qualified PostgreSQL.Binary.Encoding.Builders as B
import qualified Data.ByteString.Builder as M
import qualified Data.ByteString.Lazy as N
import qualified Data.Text.Lazy as L
import qualified Network.IP.Addr as G


-- * Value
-------------------------

{-|
The final encoded value bytes.
-}
newtype Value =
  Value { valueBytes :: ByteString {- ^ Get the final encoded strict bytes of a value. -} }

{-|
Turn an encoded primitive into value.
-}
{-# INLINE primitiveValue #-}
primitiveValue :: Primitive -> Value
primitiveValue =
  Value . C.builderBytes . primitiveBuilder

{-|
Turn an array builder into final value.
The first parameter is Array OID.
-}
{-# INLINE arrayValue #-}
arrayValue :: Word32 -> Array -> Value
arrayValue oid (Array payload dimensions nulls) =
  Value (C.builderBytes (B.array oid dimensions nulls payload))

{-|
A helper for encoding of arrays of single dimension from foldables.
The first parameter is Array OID.
-}
{-# INLINE array_foldable #-}
array_foldable :: Foldable foldable => Word32 -> (element -> Maybe Primitive) -> foldable element -> Value
array_foldable oid elementPrimitive =
  arrayValue oid . dimensionArray (maybe nullArray primitiveArray . elementPrimitive)

{-|
A helper for encoding of arrays of single dimension from vectors.
The first parameter is Array OID.
-}
{-# INLINE array_vector #-}
array_vector :: Word32 -> (element -> Primitive) -> Vector element -> Value
array_vector oid elementPrimitive vector =
  Value (C.builderBytes (B.array_vector oid (primitiveBuilder . elementPrimitive) vector))

{-|
A helper for encoding of arrays of single dimension from vectors.
The first parameter is Array OID.
-}
{-# INLINE nullableArray_vector #-}
nullableArray_vector :: Word32 -> (element -> Primitive) -> Vector (Maybe element) -> Value
nullableArray_vector oid elementPrimitive vector =
  Value (C.builderBytes (B.arrayFromNullableVector oid (primitiveBuilder . elementPrimitive) vector))

{-|
A polymorphic @HSTORE@ encoder.
-}
{-# INLINE hStore_foldable #-}
hStore_foldable :: Foldable foldable => foldable (Text, Maybe Text) -> Value
hStore_foldable =
  Value . C.builderBytes . B.hStoreUsingFoldl foldl

{-|
@HSTORE@ encoder from HashMap.
-}
{-# INLINE hStore_hashMap #-}
hStore_hashMap :: HashMap Text (Maybe Text) -> Value
hStore_hashMap =
  Value . C.builderBytes . B.hStore_hashMap

{-|
@HSTORE@ encoder from Map.
-}
{-# INLINE hStore_map #-}
hStore_map :: Map Text (Maybe Text) -> Value
hStore_map =
  Value . C.builderBytes . B.hStore_map


-- * Primitive
-------------------------

{-|

-}
newtype Primitive =
  Primitive { primitiveBuilder :: C.Builder }

{-# INLINE bool #-}
bool :: Bool -> Primitive
bool =
  Primitive . B.bool

{-# INLINE int2_int16 #-}
int2_int16 :: Int16 -> Primitive
int2_int16 =
  Primitive . B.int2_int16

{-# INLINE int2_word16 #-}
int2_word16 :: Word16 -> Primitive
int2_word16 =
  Primitive . B.int2_word16

{-# INLINE int4_int32 #-}
int4_int32 :: Int32 -> Primitive
int4_int32 =
  Primitive . B.int4_int32

{-# INLINE int4_word32 #-}
int4_word32 :: Word32 -> Primitive
int4_word32 =
  Primitive . B.int4_word32

{-# INLINE int8_int64 #-}
int8_int64 :: Int64 -> Primitive
int8_int64 =
  Primitive . B.int8_int64

{-# INLINE int8_word64 #-}
int8_word64 :: Word64 -> Primitive
int8_word64 =
  Primitive . B.int8_word64

{-# INLINE float4 #-}
float4 :: Float -> Primitive
float4 =
  Primitive . B.float4

{-# INLINE float8 #-}
float8 :: Double -> Primitive
float8 =
  Primitive . B.float8

{-# INLINE numeric #-}
numeric :: Scientific -> Primitive
numeric =
  Primitive . B.numeric

{-# INLINE uuid #-}
uuid :: UUID -> Primitive
uuid =
  Primitive . B.uuid

{-# INLINE inet #-}
inet :: G.NetAddr G.IP -> Primitive
inet =
  Primitive . B.inet

{-# INLINE char_utf8 #-}
char_utf8 :: Char -> Primitive
char_utf8 =
  Primitive . B.char_utf8

{-# INLINE text_strict #-}
text_strict :: Text -> Primitive
text_strict =
  Primitive . B.text_strict

{-# INLINE text_lazy #-}
text_lazy :: L.Text -> Primitive
text_lazy =
  Primitive . B.text_lazy

{-# INLINE bytea_strict #-}
bytea_strict :: ByteString -> Primitive
bytea_strict =
  Primitive . B.bytea_strict

{-# INLINE bytea_lazy #-}
bytea_lazy :: N.ByteString -> Primitive
bytea_lazy =
  Primitive . B.bytea_lazy

{-# INLINE bytea_lazyBuilder #-}
bytea_lazyBuilder :: M.Builder -> Primitive
bytea_lazyBuilder =
  Primitive . B.bytea_lazyBuilder

{-# INLINE date #-}
date :: Day -> Primitive
date =
  Primitive . B.date

{-# INLINE time_int #-}
time_int :: TimeOfDay -> Primitive
time_int =
  Primitive . B.time_int

{-# INLINE time_float #-}
time_float :: TimeOfDay -> Primitive
time_float =
  Primitive . B.time_float

{-# INLINE timetz_int #-}
timetz_int :: (TimeOfDay, TimeZone) -> Primitive
timetz_int =
  Primitive . B.timetz_int

{-# INLINE timetz_float #-}
timetz_float :: (TimeOfDay, TimeZone) -> Primitive
timetz_float =
  Primitive . B.timetz_float

{-# INLINE tz #-}
tz :: TimeZone -> Primitive
tz =
  Primitive . B.tz

{-# INLINE timestamp_int #-}
timestamp_int :: LocalTime -> Primitive
timestamp_int =
  Primitive . B.timestamp_int

{-# INLINE timestamp_float #-}
timestamp_float :: LocalTime -> Primitive
timestamp_float =
  Primitive . B.timestamp_float

{-# INLINE timestamptz_int #-}
timestamptz_int :: UTCTime -> Primitive
timestamptz_int =
  Primitive . B.timestamptz_int

{-# INLINE timestamptz_float #-}
timestamptz_float :: UTCTime -> Primitive
timestamptz_float =
  Primitive . B.timestamptz_float

{-# INLINE interval_int #-}
interval_int :: DiffTime -> Primitive
interval_int =
  Primitive . B.interval_int

{-# INLINE interval_float #-}
interval_float :: DiffTime -> Primitive
interval_float =
  Primitive . B.interval_float

{-# INLINE json_bytes #-}
json_bytes :: ByteString -> Primitive
json_bytes =
  Primitive . B.json_bytes

{-# INLINE jsonb_bytes #-}
jsonb_bytes :: ByteString -> Primitive
jsonb_bytes =
  Primitive . B.jsonb_bytes


-- * Array
-------------------------

{-|
Abstraction for encoding into multidimensional array.
-}
data Array =
  Array !C.Builder ![Int32] !Bool

primitiveArray :: Primitive -> Array
primitiveArray primitive =
  Array (B.sized (primitiveBuilder primitive)) [] False

nullArray :: Array
nullArray =
  Array B.null4 [] True

dimensionArray :: Foldable foldable => (a -> Array) -> foldable a -> Array
dimensionArray elementArray input =
  Array builder dimensions nulls
  where
    dimensions =
      foldedLength : foldedDimensions
    (builder, foldedDimensions, foldedLength, nulls) =
      foldl' step init input
      where
        init =
          (mempty, [], 0, False)
        step (!builder, _, !length, !nulls) element =
          (builder <> elementBuilder, elementDimensions, succ length, nulls || elementNulls)
          where
            Array elementBuilder elementDimensions elementNulls =
              elementArray element

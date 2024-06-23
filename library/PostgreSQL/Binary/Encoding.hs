module PostgreSQL.Binary.Encoding
  ( -- * Encoding
    Encoding,
    encodingBytes,
    composite,
    array,
    array_foldable,
    array_vector,
    nullableArray_vector,
    hStore_foldable,
    hStore_hashMap,
    hStore_map,

    -- * Primitives
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

    -- ** Time

    -- | Some of the functions in this section are distinguished based
    -- on the @integer_datetimes@ setting of the server.
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

    -- ** JSON
    json_bytes,
    json_bytes_lazy,
    json_ast,
    jsonb_bytes,
    jsonb_bytes_lazy,
    jsonb_ast,

    -- * Array
    Array,
    encodingArray,
    nullArray,
    dimensionArray,

    -- * Composite
    Composite,
    field,
    nullField,
  )
where

import qualified ByteString.StrictBuilder as C
import qualified Data.Aeson as R
import qualified Data.ByteString.Lazy as N
import qualified Data.IP as G
import qualified Data.Text.Lazy as L
import qualified PostgreSQL.Binary.Encoding.Builders as B
import PostgreSQL.Binary.Prelude hiding (bool, length)

type Encoding =
  C.Builder

{-# INLINE encodingBytes #-}
encodingBytes :: Encoding -> ByteString
encodingBytes =
  C.builderBytes

-- * Values

{-# INLINE composite #-}
composite :: Composite -> Encoding
composite (Composite size fields) =
  B.int4_int size <> fields

-- |
-- Turn an array builder into final value.
-- The first parameter is OID of the element type.
{-# INLINE array #-}
array :: Word32 -> Array -> Encoding
array oid (Array payload dimensions nulls) =
  B.array oid dimensions nulls payload

-- |
-- A helper for encoding of arrays of single dimension from foldables.
-- The first parameter is OID of the element type.
{-# INLINE array_foldable #-}
array_foldable :: (Foldable foldable) => Word32 -> (element -> Maybe Encoding) -> foldable element -> Encoding
array_foldable oid elementBuilder =
  array oid . dimensionArray foldl' (maybe nullArray encodingArray . elementBuilder)

-- |
-- A helper for encoding of arrays of single dimension from vectors.
-- The first parameter is OID of the element type.
{-# INLINE array_vector #-}
array_vector :: Word32 -> (element -> Encoding) -> Vector element -> Encoding
array_vector oid elementBuilder vector =
  B.array_vector oid elementBuilder vector

-- |
-- A helper for encoding of arrays of single dimension from vectors.
-- The first parameter is OID of the element type.
{-# INLINE nullableArray_vector #-}
nullableArray_vector :: Word32 -> (element -> Encoding) -> Vector (Maybe element) -> Encoding
nullableArray_vector oid elementBuilder vector =
  B.nullableArray_vector oid elementBuilder vector

-- |
-- A polymorphic @HSTORE@ encoder.
{-# INLINE hStore_foldable #-}
hStore_foldable :: (Foldable foldable) => foldable (Text, Maybe Text) -> Encoding
hStore_foldable =
  B.hStoreUsingFoldl foldl

-- |
-- @HSTORE@ encoder from HashMap.
{-# INLINE hStore_hashMap #-}
hStore_hashMap :: HashMap Text (Maybe Text) -> Encoding
hStore_hashMap =
  B.hStore_hashMap

-- |
-- @HSTORE@ encoder from Map.
{-# INLINE hStore_map #-}
hStore_map :: Map Text (Maybe Text) -> Encoding
hStore_map =
  B.hStore_map

-- * Primitive

{-# INLINE bool #-}
bool :: Bool -> Encoding
bool =
  B.bool

{-# INLINE int2_int16 #-}
int2_int16 :: Int16 -> Encoding
int2_int16 =
  B.int2_int16

{-# INLINE int2_word16 #-}
int2_word16 :: Word16 -> Encoding
int2_word16 =
  B.int2_word16

{-# INLINE int4_int32 #-}
int4_int32 :: Int32 -> Encoding
int4_int32 =
  B.int4_int32

{-# INLINE int4_word32 #-}
int4_word32 :: Word32 -> Encoding
int4_word32 =
  B.int4_word32

{-# INLINE int8_int64 #-}
int8_int64 :: Int64 -> Encoding
int8_int64 =
  B.int8_int64

{-# INLINE int8_word64 #-}
int8_word64 :: Word64 -> Encoding
int8_word64 =
  B.int8_word64

{-# INLINE float4 #-}
float4 :: Float -> Encoding
float4 =
  B.float4

{-# INLINE float8 #-}
float8 :: Double -> Encoding
float8 =
  B.float8

{-# INLINE numeric #-}
numeric :: Scientific -> Encoding
numeric =
  B.numeric

{-# INLINE uuid #-}
uuid :: UUID -> Encoding
uuid =
  B.uuid

{-# INLINE inet #-}
inet :: G.IPRange -> Encoding
inet =
  B.inet

{-# INLINE char_utf8 #-}
char_utf8 :: Char -> Encoding
char_utf8 =
  B.char_utf8

{-# INLINE text_strict #-}
text_strict :: Text -> Encoding
text_strict =
  B.text_strict

{-# INLINE text_lazy #-}
text_lazy :: L.Text -> Encoding
text_lazy =
  B.text_lazy

{-# INLINE bytea_strict #-}
bytea_strict :: ByteString -> Encoding
bytea_strict =
  B.bytea_strict

{-# INLINE bytea_lazy #-}
bytea_lazy :: N.ByteString -> Encoding
bytea_lazy =
  B.bytea_lazy

{-# INLINE date #-}
date :: Day -> Encoding
date =
  B.date

{-# INLINE time_int #-}
time_int :: TimeOfDay -> Encoding
time_int =
  B.time_int

{-# INLINE time_float #-}
time_float :: TimeOfDay -> Encoding
time_float =
  B.time_float

{-# INLINE timetz_int #-}
timetz_int :: (TimeOfDay, TimeZone) -> Encoding
timetz_int =
  B.timetz_int

{-# INLINE timetz_float #-}
timetz_float :: (TimeOfDay, TimeZone) -> Encoding
timetz_float =
  B.timetz_float

{-# INLINE timestamp_int #-}
timestamp_int :: LocalTime -> Encoding
timestamp_int =
  B.timestamp_int

{-# INLINE timestamp_float #-}
timestamp_float :: LocalTime -> Encoding
timestamp_float =
  B.timestamp_float

{-# INLINE timestamptz_int #-}
timestamptz_int :: UTCTime -> Encoding
timestamptz_int =
  B.timestamptz_int

{-# INLINE timestamptz_float #-}
timestamptz_float :: UTCTime -> Encoding
timestamptz_float =
  B.timestamptz_float

{-# INLINE interval_int #-}
interval_int :: DiffTime -> Encoding
interval_int =
  B.interval_int

{-# INLINE interval_float #-}
interval_float :: DiffTime -> Encoding
interval_float =
  B.interval_float

{-# INLINE json_bytes #-}
json_bytes :: ByteString -> Encoding
json_bytes =
  B.json_bytes

{-# INLINE json_bytes_lazy #-}
json_bytes_lazy :: N.ByteString -> Encoding
json_bytes_lazy =
  B.json_bytes_lazy

{-# INLINE json_ast #-}
json_ast :: R.Value -> Encoding
json_ast =
  B.json_ast

{-# INLINE jsonb_bytes #-}
jsonb_bytes :: ByteString -> Encoding
jsonb_bytes =
  B.jsonb_bytes

{-# INLINE jsonb_bytes_lazy #-}
jsonb_bytes_lazy :: N.ByteString -> Encoding
jsonb_bytes_lazy =
  B.jsonb_bytes_lazy

{-# INLINE jsonb_ast #-}
jsonb_ast :: R.Value -> Encoding
jsonb_ast =
  B.jsonb_ast

-- * Array

-- |
-- Abstraction for encoding into multidimensional array.
data Array
  = Array !Encoding ![Int32] !Bool

encodingArray :: Encoding -> Array
encodingArray value =
  Array (B.sized value) [] False

nullArray :: Array
nullArray =
  Array B.null4 [] True

dimensionArray :: (forall b. (b -> a -> b) -> b -> c -> b) -> (a -> Array) -> c -> Array
dimensionArray foldl' elementArray input =
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

-- * Composite

data Composite
  = Composite !Int !Encoding

instance Semigroup Composite where
  Composite lSize lFields <> Composite rSize rFields =
    Composite (lSize + rSize) (lFields <> rFields)

instance Monoid Composite where
  mempty = Composite 0 mempty

field :: Word32 -> Encoding -> Composite
field oid value =
  Composite 1 (B.int4_word32 oid <> B.sized value)

nullField :: Word32 -> Composite
nullField oid =
  Composite 1 (B.int4_word32 oid <> B.null4)

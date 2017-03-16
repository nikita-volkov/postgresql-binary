module PostgreSQL.Binary.Encoding
(
  valueBytes,

  -- * Value
  Value,
  primitiveValue,
  arrayValue,
  arrayFromFoldable,
  arrayFromVector,
  nullableArrayFromVector,
  hStoreFromFoldable,
  hStoreFromHashMap,
  hStoreFromMap,

  -- * Primitive
  Primitive,
  bool,
  int2FromInt16,
  int2FromWord16,
  int4FromInt32,
  int4FromWord32,
  int8FromInt64,
  int8FromWord64,
  float4,
  float8,
  numeric,
  uuid,
  inet,
  charInUTF8,
  textFromStrict,
  textFromLazy,
  byteaFromStrict,
  byteaFromLazy,
  byteaFromLazyBuilder,
  -- ** Time
  -- | Some of the functions in this section are distinguished based
  -- on the @integer_datetimes@ setting of the server.
  date,
  intTime,
  floatTime,
  intTimetz,
  floatTimetz,
  tz,
  intTimestamp,
  floatTimestamp,
  intTimestamptz,
  floatTimestamptz,
  intInterval,
  floatInterval,
  -- ** JSON
  jsonFromBytes,
  jsonbFromBytes,

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
{-# INLINE arrayFromFoldable #-}
arrayFromFoldable :: Foldable foldable => Word32 -> (element -> Maybe Primitive) -> foldable element -> Value
arrayFromFoldable oid elementPrimitive =
  arrayValue oid . dimensionArray (maybe nullArray primitiveArray . elementPrimitive)

{-|
A helper for encoding of arrays of single dimension from vectors.
The first parameter is Array OID.
-}
{-# INLINE arrayFromVector #-}
arrayFromVector :: Word32 -> (element -> Primitive) -> Vector element -> Value
arrayFromVector oid elementPrimitive vector =
  Value (C.builderBytes (B.arrayFromVector oid (primitiveBuilder . elementPrimitive) vector))

{-|
A helper for encoding of arrays of single dimension from vectors.
The first parameter is Array OID.
-}
{-# INLINE nullableArrayFromVector #-}
nullableArrayFromVector :: Word32 -> (element -> Primitive) -> Vector (Maybe element) -> Value
nullableArrayFromVector oid elementPrimitive vector =
  Value (C.builderBytes (B.arrayFromNullableVector oid (primitiveBuilder . elementPrimitive) vector))

{-|
A polymorphic @HSTORE@ encoder.
-}
{-# INLINE hStoreFromFoldable #-}
hStoreFromFoldable :: Foldable foldable => foldable (Text, Maybe Text) -> Value
hStoreFromFoldable =
  Value . C.builderBytes . B.hStoreUsingFoldl foldl

{-|
@HSTORE@ encoder from HashMap.
-}
{-# INLINE hStoreFromHashMap #-}
hStoreFromHashMap :: HashMap Text (Maybe Text) -> Value
hStoreFromHashMap =
  Value . C.builderBytes . B.hStoreFromHashMap

{-|
@HSTORE@ encoder from Map.
-}
{-# INLINE hStoreFromMap #-}
hStoreFromMap :: Map Text (Maybe Text) -> Value
hStoreFromMap =
  Value . C.builderBytes . B.hStoreFromMap


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

{-# INLINE int2FromInt16 #-}
int2FromInt16 :: Int16 -> Primitive
int2FromInt16 =
  Primitive . B.int2FromInt16

{-# INLINE int2FromWord16 #-}
int2FromWord16 :: Word16 -> Primitive
int2FromWord16 =
  Primitive . B.int2FromWord16

{-# INLINE int4FromInt32 #-}
int4FromInt32 :: Int32 -> Primitive
int4FromInt32 =
  Primitive . B.int4FromInt32

{-# INLINE int4FromWord32 #-}
int4FromWord32 :: Word32 -> Primitive
int4FromWord32 =
  Primitive . B.int4FromWord32

{-# INLINE int8FromInt64 #-}
int8FromInt64 :: Int64 -> Primitive
int8FromInt64 =
  Primitive . B.int8FromInt64

{-# INLINE int8FromWord64 #-}
int8FromWord64 :: Word64 -> Primitive
int8FromWord64 =
  Primitive . B.int8FromWord64

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

{-# INLINE charInUTF8 #-}
charInUTF8 :: Char -> Primitive
charInUTF8 =
  Primitive . B.charInUTF8

{-# INLINE textFromStrict #-}
textFromStrict :: Text -> Primitive
textFromStrict =
  Primitive . B.textFromStrict

{-# INLINE textFromLazy #-}
textFromLazy :: L.Text -> Primitive
textFromLazy =
  Primitive . B.textFromLazy

{-# INLINE byteaFromStrict #-}
byteaFromStrict :: ByteString -> Primitive
byteaFromStrict =
  Primitive . B.byteaFromStrict

{-# INLINE byteaFromLazy #-}
byteaFromLazy :: N.ByteString -> Primitive
byteaFromLazy =
  Primitive . B.byteaFromLazy

{-# INLINE byteaFromLazyBuilder #-}
byteaFromLazyBuilder :: M.Builder -> Primitive
byteaFromLazyBuilder =
  Primitive . B.byteaFromLazyBuilder

{-# INLINE date #-}
date :: Day -> Primitive
date =
  Primitive . B.date

{-# INLINE intTime #-}
intTime :: TimeOfDay -> Primitive
intTime =
  Primitive . B.intTime

{-# INLINE floatTime #-}
floatTime :: TimeOfDay -> Primitive
floatTime =
  Primitive . B.floatTime

{-# INLINE intTimetz #-}
intTimetz :: (TimeOfDay, TimeZone) -> Primitive
intTimetz =
  Primitive . B.intTimetz

{-# INLINE floatTimetz #-}
floatTimetz :: (TimeOfDay, TimeZone) -> Primitive
floatTimetz =
  Primitive . B.floatTimetz

{-# INLINE tz #-}
tz :: TimeZone -> Primitive
tz =
  Primitive . B.tz

{-# INLINE intTimestamp #-}
intTimestamp :: LocalTime -> Primitive
intTimestamp =
  Primitive . B.intTimestamp

{-# INLINE floatTimestamp #-}
floatTimestamp :: LocalTime -> Primitive
floatTimestamp =
  Primitive . B.floatTimestamp

{-# INLINE intTimestamptz #-}
intTimestamptz :: UTCTime -> Primitive
intTimestamptz =
  Primitive . B.intTimestamptz

{-# INLINE floatTimestamptz #-}
floatTimestamptz :: UTCTime -> Primitive
floatTimestamptz =
  Primitive . B.floatTimestamptz

{-# INLINE intInterval #-}
intInterval :: DiffTime -> Primitive
intInterval =
  Primitive . B.intInterval

{-# INLINE floatInterval #-}
floatInterval :: DiffTime -> Primitive
floatInterval =
  Primitive . B.floatInterval

{-# INLINE jsonFromBytes #-}
jsonFromBytes :: ByteString -> Primitive
jsonFromBytes =
  Primitive . B.jsonFromBytes

{-# INLINE jsonbFromBytes #-}
jsonbFromBytes :: ByteString -> Primitive
jsonbFromBytes =
  Primitive . B.jsonbFromBytes


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

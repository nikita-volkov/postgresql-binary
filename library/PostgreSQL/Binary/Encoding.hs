module PostgreSQL.Binary.Encoding
where

import PostgreSQL.Binary.Prelude hiding (bool, length)
import qualified StrictBytesBuilder as C
import qualified Control.Foldl as F
import qualified Data.Vector as A
import qualified PostgreSQL.Binary.Encoding.Builders as B


-- * Value
-------------------------

{-|
The final encoded value bytes
-}
newtype Value =
  Value { valueBytes :: ByteString }

arrayValue :: Word32 -> Array -> Value
arrayValue oid (Array payload dimensions nulls) =
  Value (C.builderBytes (B.array oid dimensions nulls payload))

primitiveValue :: Primitive -> Value
primitiveValue =
  undefined

{-|
A helper for encoding of arrays of single dimension from foldables.
-}
{-# INLINE foldableValue #-}
foldableValue :: Foldable foldable => Word32 -> (element -> Maybe Primitive) -> foldable element -> Value
foldableValue oid elementPrimitive =
  arrayValue oid . dimensionArray foldl' (maybe nullArray primitiveArray . elementPrimitive)

{-|
A helper for encoding of arrays of single dimension from vectors.
-}
{-# INLINE vectorValue #-}
vectorValue :: Word32 -> (element -> Primitive) -> Vector element -> Value
vectorValue oid elementPrimitive vector =
  Value (C.builderBytes (B.arrayFromVector oid (primitiveBuilder . elementPrimitive) vector))

{-|
A helper for encoding of arrays of single dimension from vectors.
-}
{-# INLINE nullableVectorValue #-}
nullableVectorValue :: Word32 -> (element -> Primitive) -> Vector (Maybe element) -> Value
nullableVectorValue oid elementPrimitive vector =
  Value (C.builderBytes (B.arrayFromNullableVector oid (primitiveBuilder . elementPrimitive) vector))

{-|
An optimized helper for directly encoding into 'ByteString'.
-}
textValue :: Text -> Value
textValue =
  undefined


-- * Primitive
-------------------------

newtype Primitive =
  Primitive { primitiveBuilder :: C.Builder }

int4FromInt32 :: Int32 -> Primitive
int4FromInt32 =
  undefined

int4FromInt :: Int -> Primitive
int4FromInt =
  undefined

{-|
Encodes text using builder.
Use this when you need to compose with arrays and such.
Otherwise use 'textValue', which is optimized to directly encode into 'ByteString'.
-}
textPrimitive :: Text -> Primitive
textPrimitive =
  undefined


-- * Array
-------------------------

data Array =
  Array !C.Builder ![Int32] !Bool

primitiveArray :: Primitive -> Array
primitiveArray primitive =
  Array (B.sized (primitiveBuilder primitive)) [] False

nullArray :: Array
nullArray =
  Array B.null4 [] True

dimensionArray :: (forall a. (a -> b -> a) -> a -> c -> a) -> (b -> Array) -> c -> Array
dimensionArray foldl elementArray input =
  Array builder dimensions nulls
  where
    dimensions =
      foldedLength : foldedDimensions
    (builder, foldedDimensions, foldedLength, nulls) =
      foldl step init input
      where
        init =
          (mempty, [], 0, False)
        step (!builder, _, !length, !nulls) element =
          (builder <> elementBuilder, elementDimensions, succ length, nulls || elementNulls)
          where
            Array elementBuilder elementDimensions elementNulls =
              elementArray element

-- |
-- An intermediate array representation
-- and utilities for its (de)composition.
-- 
-- Inspired by the following:
-- 
-- * <http://libpqtypes.esilo.com/man3/pqt-specs.html>
-- 
-- * <http://libpqtypes.esilo.com/browse_source.html?file=array.c>
module PostgreSQLBinary.ArrayData where

import PostgreSQLBinary.Prelude hiding (Data)
import qualified Data.Vector as V


-- | 
-- A representation of a data serializable to the PostgreSQL array binary format.
-- 
-- Consists of a list of dimensions, a list of encoded elements,
-- a flag specifying, whether it contains any nulls, and an oid.
type Data = ([Dimension], [Value], Bool, Word32)

-- | 
-- A width and a lower bound.
type Dimension = (Word32, Word32)

-- |
-- An encoded value. 'Nothing' if it represents a @NULL@.
type Value = Maybe ByteString

-- |
-- Access a value of data, if the data represents a single value.
asSingleton :: Data -> Maybe Value
asSingleton (dimensions, elements, nulls, oid) =
  if null dimensions
    then case elements of
      [x] -> return x
      l -> $bug $ "Unexpected amount of elements: " <> show l
    else mzero

-- |
-- Construct from a non-empty list,
-- taking the shared parameters from the first element.
fromListUnsafe :: [Data] -> Data
fromListUnsafe list =
  case list of
    (dimensions, values, nulls, oid) : tail ->
      ((fromIntegral $ length list, 1) : dimensions, values <> foldMap valuesOf tail, nulls, oid)
      where
        valuesOf (_, x, _, _) = x
    _ ->
      error "Empty list"

fromSingleton :: Value -> Bool -> Word32 -> Data
fromSingleton value nullable oid =
  ([], [value], nullable, oid)

-- |
-- Get a list of elements.
elements :: Data -> [Data]
elements (dimensions, values, nulls, oid) =
  do
    subvalues <- slice values
    return (subdimensions, subvalues, nulls, oid)
  where
    ((width, lowerBound), subdimensions) =
      case dimensions of
        h : t -> (h, t)
        _ -> ((0, 1), [])
    chunkSize =
      if null subdimensions
        then 1
        else product $ map dimensionWidth $ subdimensions
      where
        dimensionWidth (x, _) = fromIntegral x
    slice = 
      foldr (\f g l -> case f l of (a, b) -> a : g b) (const mzero) $ 
      replicate (fromIntegral width) (splitAt chunkSize)

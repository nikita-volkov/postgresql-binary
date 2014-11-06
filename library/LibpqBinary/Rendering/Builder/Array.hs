-- |
-- Inspired by the following:
-- 
-- * <http://libpqtypes.esilo.com/man3/pqt-specs.html>
-- 
-- * <http://libpqtypes.esilo.com/browse_source.html?file=array.c>
module LibpqBinary.Rendering.Builder.Array where

import LibpqBinary.Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.Vector as V


-- | A vector of dimensions and a stream of elements.
type Array = (Vector Dimension, [Element], Bool, Int32)

-- | A lower bound and a width
type Dimension = (Word32, Word32)

-- | A data length and a builder
type Element = (Word32, BB.Builder)


array :: Array -> BB.Builder
array (dimensionsV, elementsV, hasNullV, oidV) =
  dimsLength <> hasNull <> oid <> dimensions <> elements
  where
    dimsLength = BB.int32BE $ fromIntegral $ V.length dimensionsV
    hasNull = case hasNullV of True -> BB.word8 1; False -> BB.word8 0
    oid = BB.int32BE oidV
    dimensions = V.foldl' (<>) mempty $ V.map dimension $ dimensionsV
    elements = (mconcat . map element) elementsV

dimension :: Dimension -> BB.Builder
dimension (l, w) = BB.word32BE w <> BB.word32BE l

element :: Element -> BB.Builder
element (l, b) = BB.word32BE l <> b


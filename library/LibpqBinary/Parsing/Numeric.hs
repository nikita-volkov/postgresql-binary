-- |
-- Utils for dealing with numbers.
module LibpqBinary.Parsing.Numeric where

import LibpqBinary.Prelude
import qualified Data.ByteString as B


{-# INLINE byteSize #-}
byteSize :: (Bits a) => a -> Int
byteSize = (`div` 8) . bitSize

{-# INLINE pack #-}
pack :: (Bits a, Num a) => B.ByteString -> a
pack = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0


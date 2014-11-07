-- |
-- Utils for dealing with numbers.
module PostgreSQLBinary.Integral where

import PostgreSQLBinary.Prelude
import qualified Data.ByteString as B


{-# INLINE byteSize #-}
byteSize :: (Bits a) => a -> Int
byteSize = (`div` 8) . bitSize

{-# INLINE pack #-}
pack :: (Bits a, Num a) => B.ByteString -> a
pack = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

{-# INLINE unpack #-}
unpack :: (Bits a, Integral a) => a -> B.ByteString
unpack x = B.pack $ map f $ reverse [0..byteSize x - 1]
  where f s = fromIntegral $ shiftR x (8 * s)

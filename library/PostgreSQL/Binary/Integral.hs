-- |
-- Utils for dealing with integer numbers.
module PostgreSQL.Binary.Integral where

import PostgreSQL.Binary.Prelude
import qualified Data.ByteString as B


{-# INLINABLE pack #-}
pack :: (Bits a, Num a) => B.ByteString -> a
pack =
  B.foldl' (\n h -> shiftL n 8 .|. fromIntegral h) 0

{-# INLINE unpackBySize #-}
unpackBySize :: (Bits a, Integral a) => Int -> a -> B.ByteString
unpackBySize n x =
  B.pack $ map f $ reverse [0..n - 1]
  where 
    f s =
      fromIntegral $ shiftR x (8 * s)

-- |
-- Utils for dealing with numbers.
module PostgreSQLBinary.Integral where

import PostgreSQLBinary.Prelude
import qualified Data.ByteString as B


{-# INLINE pack #-}
pack :: (Bits a, Num a) => B.ByteString -> a
pack = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

module PostgreSQLBinary.Decoder where

import PostgreSQLBinary.Prelude
import qualified PostgreSQLBinary.Decoder.Zepto as Zepto


-- |
-- A function for decoding a byte string into a value.
type D a = ByteString -> Either Text a


{-# INLINABLE numeric #-}
numeric :: D Scientific
numeric =
  flip Zepto.run (inline Zepto.numeric)


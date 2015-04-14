{-# LANGUAGE OverloadedStrings #-}
module PostgreSQLBinary.Composite
  ( Field(..)
  , parseField
  , createField
  ) where

import qualified Data.ByteString as B

import PostgreSQLBinary.Prelude

data Field
  = Field { fieldOid  :: {-# UNPACK #-} !Word32
            -- | For convenience this is Int32. Note that PG only supports
            -- <http://doxygen.postgresql.org/datum_8c.html Datum> up to 1GiB.
          , fieldSize :: {-# UNPACK #-} !Int32
          , field     :: !ByteString }
  | NULL { fieldOid :: {-# UNPACK #-} !Word32 }
 deriving (Show, Eq, Ord)

{-# INLINE parseField #-}
parseField :: (Word32 -> ByteString -> Either Text a) -> Field -> Either Text a
parseField parse f =
  case f of
    Field oid _ bs -> parse oid bs
    NULL _oid      -> Left "parseField: NULL"

createField :: Word32 -> Maybe ByteString -> Field
createField oid mbs = case mbs of
  Just f  -> Field oid (fromIntegral (B.length f)) f
  Nothing -> NULL oid

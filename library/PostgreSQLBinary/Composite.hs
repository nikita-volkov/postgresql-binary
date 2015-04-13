{-# LANGUAGE OverloadedStrings #-}
module PostgreSQLBinary.Composite
  ( Field(..)
  , parseField
  ) where
import PostgreSQLBinary.Prelude

data Field
  = Field { fieldOid  :: {-# UNPACK #-} !Int32
          , fieldSize :: {-# UNPACK #-} !Int32
          , field     :: !ByteString }
  | NULL { fieldOid :: {-# UNPACK #-} !Int32 }
 deriving (Show, Eq, Ord)

{-# INLINE parseField #-}
parseField :: (Int32 -> ByteString -> Either Text a) -> Field -> Either Text a
parseField parse f =
  case f of
    Field oid _ bs -> parse oid bs
    NULL _oid      -> Left "parseField: NULL"

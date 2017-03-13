module PostgreSQL.Binary.Inet where

import PostgreSQL.Binary.Prelude


-- | Address family AF_INET
inetAddressFamily :: Word8
inetAddressFamily =
  2

-- | Address family AF_INET6
inet6AddressFamily :: Word8
inet6AddressFamily =
  3

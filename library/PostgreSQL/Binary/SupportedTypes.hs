{-|
Reexports of all the data-types that this API supports.
Useful for the reduction of dependencies in the \"postgresql-binary\" dependent libraries.
-}
module PostgreSQL.Binary.SupportedTypes
(
  Data.Vector.Vector,
  Data.Scientific.Scientific,
)
where

import Data.Vector
import Data.Scientific
import Data.UUID
import Network.IP.Addr
import PostgreSQL.Binary.Prelude
import PostgreSQL.Binary.Numeric

{-|
Reexports of all the data-types that this API supports.
Useful for the reduction of dependencies in the \"postgresql-binary\" dependent libraries.
-}
module PostgreSQL.Binary.Data
(
  module Data.HashMap.Strict,
  module Data.Map.Strict,
  module Data.Scientific,
  module Data.Time,
  module Data.UUID,
  module Data.Vector,
  module Data.Aeson,
  module Network.IP.Addr,
)
where

import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map)
import Data.Scientific (Scientific)
import Data.Time (Day, TimeOfDay, TimeZone, LocalTime, UTCTime, DiffTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Aeson (Value)
import Network.IP.Addr (IP, NetAddr)

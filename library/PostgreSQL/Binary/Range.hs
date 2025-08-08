module PostgreSQL.Binary.Range where

import PostgreSQL.Binary.Prelude

data Range a = Empty | Range !(Bound a) !(Bound a)
  deriving (Eq, Show, Ord, Generic)

data Bound a = Incl !a | Excl !a | Inf
  deriving (Eq, Show, Ord, Generic)

type Multirange a = [Range a]
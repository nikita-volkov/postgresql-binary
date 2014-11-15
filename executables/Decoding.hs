module Main where

import BasePrelude
import MTLPrelude
import Control.DeepSeq
import Criterion
import Criterion.Main
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.Time
import qualified PostgreSQLBinary.Encoder as E
import qualified PostgreSQLBinary.Decoder as D
import qualified PostgreSQLBinary.Array as Array


main =
  defaultMain
    [
      b "bool"        D.bool               (E.bool True),
      b "int2"        (D.int :: D.D Int16) (E.int2 (Left 1000)),
      b "int4"        (D.int :: D.D Int32) (E.int4 (Left 1000)),
      b "int8"        (D.int :: D.D Int64) (E.int8 (Left 1000)),
      b "float4"      D.float4             (E.float4 12.65468468),
      b "float8"      D.float8             (E.float8 12.65468468),
      b "numeric"     D.numeric            (E.numeric (read "20.213290183")),
      b "char"        D.char               (E.char 'Я'),
      b "text"        D.text               (E.text (Left "alsdjflskjдывлоаы оады")),
      b "bytea"       D.bytea              (E.bytea (Left "alskdfj;dasjfl;dasjflksdj")),
      b "date"        D.date               (E.date (read "2000-01-19")),
      b "time"        (D.time True)        (E.time True (read "10:41:06")),
      b "timetz"      (D.timetz True)      (E.timetz True (read "(10:41:06, +0300)")),
      b "timestamp"   D.timestamp          (E.timestamp (read "2000-01-19 10:41:06")),
      b "timestamptz" D.timestamptz        (E.timestamptz (read "2000-01-19 10:41:06")),
      b "interval"    D.interval           (E.interval (secondsToDiffTime 23472391128374)),
      b "uuid"        D.uuid               (E.uuid (read "550e8400-e29b-41d4-a716-446655440000")),
      b "array"       D.array              (E.array 
                                             (Array.fromListUnsafe 
                                               [Array.fromSingleton (Just "dfs") True 0,
                                                Array.fromSingleton Nothing True 0]))
    ]
  where
    b name decoder value = 
      bench name $ nf decoder value

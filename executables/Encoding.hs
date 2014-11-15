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
      b "bool"        E.bool          True,
      b "int2"        E.int2          (Left 1000),
      b "int4"        E.int4          (Left 1000),
      b "int8"        E.int8          (Left 1000),
      b "float4"      E.float4        12.65468468,
      b "float8"      E.float8        12.65468468,
      b "numeric"     E.numeric       (read "20.213290183"),
      b "char"        E.char          'Я',
      b "text"        E.text          (Left "alsdjflskjдывлоаы оады"),
      b "bytea"       E.bytea         (Left "alskdfj;dasjfl;dasjflksdj"),
      b "date"        E.date          (read "2000-01-19"),
      b "time"        (E.time True)   (read "10:41:06"),
      b "timetz"      (E.timetz True) (read "(10:41:06, +0300)"),
      b "timestamp"   E.timestamp     (read "2000-01-19 10:41:06"),
      b "timestamptz" E.timestamptz   (read "2000-01-19 10:41:06"),
      b "interval"    E.interval      (secondsToDiffTime 23472391128374),
      b "uuid"        E.uuid          (read "550e8400-e29b-41d4-a716-446655440000"),
      b "array"       E.array         (Array.fromListUnsafe 
                                        [Array.fromSingleton (Just "dfs") True 0,
                                         Array.fromSingleton Nothing True 0])
    ]
  where
    b name encoder value = 
      bench name $ nf encoder value

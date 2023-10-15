module Main where

import Criterion
import Criterion.Main
import qualified PostgreSQL.Binary.Encoding as E
import Prelude

main :: IO ()
main =
  defaultMain
    [ value "bool" E.bool True,
      value "int2" E.int2_int16 1000,
      value "int4" E.int4_int32 1000,
      value "int8" E.int8_int64 1000,
      value "float4" E.float4 12.65468468,
      value "float8" E.float8 12.65468468,
      value "numeric" E.numeric (read "20.213290183"),
      value "char_utf8" E.char_utf8 'Я',
      value "text" E.text_strict "alsdjflskjдывлоаы оады",
      value "bytea" E.bytea_strict "alskdfj;dasjfl;dasjflksdj",
      value "date" E.date (read "2000-01-19"),
      value "time" E.time_int (read "10:41:06"),
      value "timetz" E.timetz_int (read "(10:41:06, +0300)"),
      value "timestamp" E.timestamp_int (read "2000-01-19 10:41:06"),
      value "timestamptz" E.timestamptz_int (read "2000-01-19 10:41:06"),
      value "interval" E.interval_int (secondsToDiffTime 23472391128374),
      value "uuid" E.uuid (read "550e8400-e29b-41d4-a716-446655440000"),
      let encoder =
            E.array 23 . E.dimensionArray foldl' (E.encodingArray . E.int4_int32)
       in value "array" encoder [1, 2, 3, 4]
    ]
  where
    value name encoder value =
      bench name $ nf (E.encodingBytes . encoder) value

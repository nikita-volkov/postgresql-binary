module Main where

import Criterion
import Criterion.Main
import qualified PostgreSQL.Binary.Decoding as D
import qualified PostgreSQL.Binary.Encoding as E
import Prelude

main =
  defaultMain
    [ b "bool" D.bool ((E.encodingBytes . E.bool) True),
      b "int2" (D.int :: D.Value Int16) ((E.encodingBytes . E.int2_int16) 1000),
      b "int4" (D.int :: D.Value Int32) ((E.encodingBytes . E.int4_int32) 1000),
      b "int8" (D.int :: D.Value Int64) ((E.encodingBytes . E.int8_int64) 1000),
      b "float4" D.float4 ((E.encodingBytes . E.float4) 12.65468468),
      b "float8" D.float8 ((E.encodingBytes . E.float8) 12.65468468),
      b "numeric" D.numeric ((E.encodingBytes . E.numeric) (read "20.213290183")),
      b "char" D.char ((E.encodingBytes . E.char_utf8) 'Я'),
      b "text" D.text_strict ((E.encodingBytes . E.text_strict) "alsdjflskjдывлоаы оады"),
      b "bytea" D.bytea_strict ((E.encodingBytes . E.bytea_strict) "alskdfj;dasjfl;dasjflksdj"),
      b "date" D.date ((E.encodingBytes . E.date) (read "2000-01-19")),
      b "time" D.time_int ((E.encodingBytes . E.time_int) (read "10:41:06")),
      b "timetz" D.timetz_int ((E.encodingBytes . E.timetz_int) (read "(10:41:06, +0300)")),
      b "timestamp" D.timestamp_int ((E.encodingBytes . E.timestamp_int) (read "2000-01-19 10:41:06")),
      b "timestamptz" D.timestamptz_int ((E.encodingBytes . E.timestamptz_int) (read "2000-01-19 10:41:06")),
      b "interval" D.interval_int ((E.encodingBytes . E.interval_int) (secondsToDiffTime 23472391128374)),
      b "uuid" D.uuid ((E.encodingBytes . E.uuid) (read "550e8400-e29b-41d4-a716-446655440000")),
      let encoder =
            E.array 23 . E.dimensionArray foldl' (E.encodingArray . E.int4_int32)
          decoder =
            D.array $
              D.dimensionArray replicateM $
                D.valueArray $
                  (D.int :: D.Value Int32)
       in b "array" decoder (E.encodingBytes (encoder [1, 2, 3, 4]))
    ]
  where
    b name decoder value =
      bench name $ nf (D.valueParser decoder) value

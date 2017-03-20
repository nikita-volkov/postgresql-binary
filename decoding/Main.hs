module Main where

import Prelude
import Criterion
import Criterion.Main
import qualified PostgreSQL.Binary.Encoding as E
import qualified PostgreSQL.Binary.Decoding as D


main =
  defaultMain
    [
      b "bool" D.bool ((E.valueBytes . E.primitiveValue . E.bool) True)
      ,
      b "int2" (D.int :: D.Value Int16) ((E.valueBytes . E.primitiveValue . E.int2_int16) 1000)
      ,
      b "int4" (D.int :: D.Value Int32) ((E.valueBytes . E.primitiveValue . E.int4_int32) 1000)
      ,
      b "int8" (D.int :: D.Value Int64) ((E.valueBytes . E.primitiveValue . E.int8_int64) 1000)
      ,
      b "float4" D.float4 ((E.valueBytes . E.primitiveValue . E.float4) 12.65468468)
      ,
      b "float8" D.float8 ((E.valueBytes . E.primitiveValue . E.float8) 12.65468468)
      ,
      b "numeric" D.numeric ((E.valueBytes . E.primitiveValue . E.numeric) (read "20.213290183"))
      ,
      b "char" D.char ((E.valueBytes . E.primitiveValue . E.char_utf8) 'Я')
      ,
      b "text" D.text_strict ((E.valueBytes . E.primitiveValue . E.text_strict) "alsdjflskjдывлоаы оады")
      ,
      b "bytea" D.bytea_strict ((E.valueBytes . E.primitiveValue . E.bytea_strict) "alskdfj;dasjfl;dasjflksdj")
      ,
      b "date" D.date ((E.valueBytes . E.primitiveValue . E.date) (read "2000-01-19"))
      ,
      b "time" D.time_int ((E.valueBytes . E.primitiveValue . E.time_int) (read "10:41:06"))
      ,
      b "timetz" D.timetz_int ((E.valueBytes . E.primitiveValue . E.timetz_int) (read "(10:41:06, +0300)"))
      ,
      b "timestamp" D.timestamp_int ((E.valueBytes . E.primitiveValue . E.timestamp_int) (read "2000-01-19 10:41:06"))
      ,
      b "timestamptz" D.timestamptz_int ((E.valueBytes . E.primitiveValue . E.timestamptz_int) (read "2000-01-19 10:41:06"))
      ,
      b "interval" D.interval_int ((E.valueBytes . E.primitiveValue . E.interval_int) (secondsToDiffTime 23472391128374))
      ,
      b "uuid" D.uuid ((E.valueBytes . E.primitiveValue . E.uuid) (read "550e8400-e29b-41d4-a716-446655440000"))
      ,
      let
        encoder =
          E.arrayValue 23 . E.dimensionArray (E.primitiveArray . E.int4_int32)
        decoder =
          D.array $
          D.dimensionArray replicateM $
          D.valueArray $
          (D.int :: D.Value Int32)
        in
          b "array" decoder (E.valueBytes (encoder [1,2,3,4]))
    ]
  where
    b name decoder value = 
      bench name $ nf (D.valueParser decoder) value

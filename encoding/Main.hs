module Main where

import Prelude
import Criterion
import Criterion.Main
import qualified PostgreSQL.Binary.Encoding as E


main =
  defaultMain
    [
      primitive "bool" E.bool True
      ,
      primitive "int2" E.int2_int16 1000
      ,
      primitive "int4" E.int4_int32 1000
      ,
      primitive "int8" E.int8_int64 1000
      ,
      primitive "float4" E.float4 12.65468468
      ,
      primitive "float8" E.float8 12.65468468
      ,
      primitive "numeric" E.numeric (read "20.213290183")
      ,
      primitive "char_utf8" E.char_utf8 'Я'
      ,
      primitive "text" E.text_strict "alsdjflskjдывлоаы оады"
      ,
      primitive "bytea" E.bytea_strict "alskdfj;dasjfl;dasjflksdj"
      ,
      primitive "date" E.date (read "2000-01-19")
      ,
      primitive "time" E.time_int (read "10:41:06")
      ,
      primitive "timetz" E.timetz_int (read "(10:41:06, +0300)")
      ,
      primitive "timestamp" E.timestamp_int (read "2000-01-19 10:41:06")
      ,
      primitive "timestamptz" E.timestamptz_int (read "2000-01-19 10:41:06")
      ,
      primitive "interval" E.interval_int (secondsToDiffTime 23472391128374)
      ,
      primitive "uuid" E.uuid (read "550e8400-e29b-41d4-a716-446655440000")
      ,
      let
        encoder =
          E.arrayValue 23 . E.dimensionArray (E.primitiveArray . E.int4_int32)
        in
          value "array" encoder [1,2,3,4]
    ]
  where
    primitive name encoder value =
      bench name $ nf (E.valueBytes . E.primitiveValue . encoder) value
    value name encoder value = 
      bench name $ nf (E.valueBytes . encoder) value

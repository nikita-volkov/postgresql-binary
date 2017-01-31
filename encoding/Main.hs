module Main where

import Prelude
import Criterion
import Criterion.Main
import qualified PostgreSQL.Binary.Encoder as E


main =
  defaultMain
    [
      b "bool" E.bool True
      ,
      b "int2" E.int2_int16 1000
      ,
      b "int4" E.int4_int32 1000
      ,
      b "int8" E.int8_int64 1000
      ,
      b "float4" E.float4 12.65468468
      ,
      b "float8" E.float8 12.65468468
      ,
      b "numeric" E.numeric (read "20.213290183")
      ,
      b "char" E.char 'Я'
      ,
      b "text" E.text_strict "alsdjflskjдывлоаы оады"
      ,
      b "bytea" E.bytea_strict "alskdfj;dasjfl;dasjflksdj"
      ,
      b "date" E.date (read "2000-01-19")
      ,
      b "time" E.time_int (read "10:41:06")
      ,
      b "timetz" E.timetz_int (read "(10:41:06, +0300)")
      ,
      b "timestamp" E.timestamp_int (read "2000-01-19 10:41:06")
      ,
      b "timestamptz" E.timestamptz_int (read "2000-01-19 10:41:06")
      ,
      b "interval" E.interval_int (secondsToDiffTime 23472391128374)
      ,
      b "uuid" E.uuid (read "550e8400-e29b-41d4-a716-446655440000")
      ,
      let
        encoder =
          E.array 23 $
          E.arrayDimension foldl' $
          E.arrayValue $
          E.int4_int32
        in
          b "array" encoder [1,2,3,4]
    ]
  where
    b name encoder value = 
      bench name $ nf (E.run encoder) value

module Main where

import Prelude
import Criterion
import Criterion.Main
import qualified PostgreSQL.Binary.Encoder as E
import qualified PostgreSQL.Binary.Decoder as D


main =
  defaultMain
    [
      b "bool" D.bool (E.run E.bool True)
      ,
      b "int2" (D.int :: D.Decoder Int16) (E.run E.int2_int16 1000)
      ,
      b "int4" (D.int :: D.Decoder Int32) (E.run E.int4_int32 1000)
      ,
      b "int8" (D.int :: D.Decoder Int64) (E.run E.int8_int64 1000)
      ,
      b "float4" D.float4 (E.run E.float4 12.65468468)
      ,
      b "float8" D.float8 (E.run E.float8 12.65468468)
      ,
      b "numeric" D.numeric (E.run E.numeric (read "20.213290183"))
      ,
      b "char" D.char (E.run E.char 'Я')
      ,
      b "text" D.text_strict (E.run E.text_strict "alsdjflskjдывлоаы оады")
      ,
      b "bytea" D.bytea_strict (E.run E.bytea_strict "alskdfj;dasjfl;dasjflksdj")
      ,
      b "date" D.date (E.run E.date (read "2000-01-19"))
      ,
      b "time" D.time_int (E.run E.time_int (read "10:41:06"))
      ,
      b "timetz" D.timetz_int (E.run E.timetz_int (read "(10:41:06, +0300)"))
      ,
      b "timestamp" D.timestamp_int (E.run E.timestamp_int (read "2000-01-19 10:41:06"))
      ,
      b "timestamptz" D.timestamptz_int (E.run E.timestamptz_int (read "2000-01-19 10:41:06"))
      ,
      b "interval" D.interval_int (E.run E.interval_int (secondsToDiffTime 23472391128374))
      ,
      b "uuid" D.uuid (E.run E.uuid (read "550e8400-e29b-41d4-a716-446655440000"))
      ,
      let
        encoder =
          E.array 23 $
          E.arrayDimension foldl' $
          E.arrayValue $
          E.int4_int32
        decoder =
          D.array $
          D.arrayDimension replicateM $
          D.arrayNonNullValue $
          (D.int :: D.Decoder Int32)
        in
          b "array" decoder (E.run encoder [1,2,3,4])
    ]
  where
    b name decoder value = 
      bench name $ nf (D.run decoder) value

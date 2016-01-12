module Main where

import Main.Prelude hiding (assert, isRight, isLeft)
import Control.Monad.IO.Class
import Test.QuickCheck.Instances
import Test.Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.SmallCheck as SmallCheck
import qualified Test.Tasty.QuickCheck as QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified PostgreSQL.Binary.Encoder as Encoder
import qualified PostgreSQL.Binary.Decoder as Decoder
import qualified Data.ByteString as ByteString
import qualified Main.DB as DB
import qualified Main.Gens as Gens
import qualified Main.Properties as Properties
import qualified Main.IO as IO
import qualified Main.PTI as PTI
import qualified Main.TextEncoder as TextEncoder 
import Main.Apx (Apx(..))


main =
  defaultMain (testGroup "" [binary, textual])

binary =
  testGroup "Binary format"
  [
    select "SELECT '1 year 2 months 3 days 4 hours 5 minutes 6 seconds 332211 microseconds' :: interval"
    (bool Decoder.interval_float Decoder.interval_int)
    (picosecondsToDiffTime (10^6 * (332211 + 10^6 * (6 + 60 * (5 + 60 * (4 + 24 * (3 + 31 * (2 + 12))))))))
    ,
    select "SELECT '10 seconds' :: interval"
    (bool Decoder.interval_float Decoder.interval_int)
    (10 :: DiffTime)
    ,
    HUnit.testCase "Interval encoder: 10 seconds" $
    let
      pti =
        PTI.interval
      encoder =
        (bool Encoder.interval_float Encoder.interval_int)
      decoder =
        (bool Decoder.interval_float Decoder.interval_int)
      value =
        (10 :: DiffTime)
      in
        HUnit.assertEqual "" (Right value) =<<
        IO.roundtrip (PTI.oidPQ (PTI.ptiOID pti)) encoder decoder value
    ,
    timeRoundtrip "interval" Gens.intervalDiffTime PTI.interval
    (bool Encoder.interval_float Encoder.interval_int)
    (bool Decoder.interval_float Decoder.interval_int)
    ,
    timeRoundtrip "timestamp" (fmap Apx Gens.auto) PTI.timestamp
    ((. unApx) . bool Encoder.timestamp_float Encoder.timestamp_int)
    (fmap Apx . bool Decoder.timestamp_float Decoder.timestamp_int)
    ,
    HUnit.testCase "timestamptz offset" $ do
      Right (textual, decoded) <-
        DB.session $ do
          integerDatetimes <- DB.integerDatetimes
          let encoder = bool Encoder.timestamptz_float Encoder.timestamptz_int integerDatetimes
              decoder = bool Decoder.timestamptz_float Decoder.timestamptz_int integerDatetimes
          DB.unit "DROP TABLE IF EXISTS a" []
          DB.unit "CREATE TABLE a (b TIMESTAMPTZ)" []
          DB.unit "set timezone to 'America/Los_Angeles'" []
          let p = (,,) (PTI.oidPQ (PTI.ptiOID PTI.timestamptz))
                       (Encoder.run encoder x) 
                       (DB.Binary)
              x = read "2011-09-28 00:17:25"
          DB.unit "insert into a (b) values ($1)" [Just p]
          DB.unit "set timezone to 'Europe/Stockholm'" []
          textual <- DB.oneRow "SELECT * FROM a" [] DB.Text
          decoded <- fmap (Decoder.run decoder) (DB.oneRow "SELECT * FROM a" [] DB.Binary)
          return (textual, decoded)
      HUnit.assertEqual "" ("2011-09-28 02:17:25+02") textual
      HUnit.assertEqual "" (Right (read "2011-09-28 00:17:25")) decoded
    ,
    timeRoundtrip "timestamptz" (fmap Apx Gens.auto) PTI.timestamptz
    ((. unApx) . bool Encoder.timestamptz_float Encoder.timestamptz_int)
    (fmap Apx . bool Decoder.timestamptz_float Decoder.timestamptz_int)
    ,
    timeRoundtrip "timetz" (fmap Apx Gens.timetz) PTI.timetz
    ((. unApx) . bool Encoder.timetz_float Encoder.timetz_int)
    (fmap Apx . bool Decoder.timetz_float Decoder.timetz_int)
    ,
    timeRoundtrip "time" (fmap Apx Gens.auto) PTI.time
    ((. unApx) . bool Encoder.time_float Encoder.time_int)
    (fmap Apx . bool Decoder.time_float Decoder.time_int)
    ,
    stdRoundtrip "numeric" Gens.scientific PTI.numeric Encoder.numeric Decoder.numeric
    ,
    select "SELECT -1234560.789 :: numeric" (const Decoder.numeric) (read "-1234560.789")
    ,
    select "SELECT -0.0789 :: numeric" (const Decoder.numeric) (read "-0.0789")
    ,
    select "SELECT 10000 :: numeric" (const Decoder.numeric) (read "10000")
    ,
    stdRoundtrip "float4" Gens.auto PTI.float4 Encoder.float4 Decoder.float4
    ,
    stdRoundtrip "float8" Gens.auto PTI.float8 Encoder.float8 Decoder.float8
    ,
    stdRoundtrip "char" Gens.char PTI.text Encoder.char Decoder.char
    ,
    stdRoundtrip "text_strict" Gens.text PTI.text Encoder.text_strict Decoder.text_strict
    ,
    stdRoundtrip "text_lazy" (fmap convert Gens.text) PTI.text Encoder.text_lazy Decoder.text_lazy
    ,
    stdRoundtrip "bytea_strict" Gens.auto PTI.bytea Encoder.bytea_strict Decoder.bytea_strict
    ,
    stdRoundtrip "bytea_lazy" Gens.auto PTI.bytea Encoder.bytea_lazy Decoder.bytea_lazy
    ,
    stdRoundtrip "uuid" Gens.uuid PTI.uuid Encoder.uuid Decoder.uuid
    ,
    stdRoundtrip "int2_int16" Gens.auto PTI.int2 Encoder.int2_int16 Decoder.int
    ,
    stdRoundtrip "int2_word16" Gens.auto PTI.int2 Encoder.int2_word16 Decoder.int
    ,
    stdRoundtrip "int4_int32" Gens.auto PTI.int4 Encoder.int4_int32 Decoder.int
    ,
    stdRoundtrip "int4_word32" Gens.auto PTI.int4 Encoder.int4_word32 Decoder.int
    ,
    stdRoundtrip "int8_int64" Gens.auto PTI.int8 Encoder.int8_int64 Decoder.int
    ,
    stdRoundtrip "int8_word64" Gens.auto PTI.int8 Encoder.int8_word64 Decoder.int
    ,
    stdRoundtrip "bool" Gens.auto PTI.bool Encoder.bool Decoder.bool
    ,
    stdRoundtrip "date" Gens.auto PTI.date Encoder.date Decoder.date
    ,
    let
      decoder =
        Decoder.array $
        Decoder.arrayDimension replicateM $
        Decoder.arrayDimension replicateM $
        Decoder.arrayNonNullValue $
        Decoder.int
      in
        select "SELECT ARRAY[ARRAY[1,2],ARRAY[3,4]]" (const decoder) ([[1,2],[3,4]] :: [[Int]])
    ,
    let
      encoder =
        Encoder.array (PTI.oidWord32 (PTI.ptiOID PTI.int8)) $
        Encoder.arrayDimension foldl' $
        Encoder.arrayDimension foldl' $
        Encoder.arrayDimension foldl' $
        Encoder.arrayValue $
        Encoder.int8_int64
      decoder =
        Decoder.array $
        Decoder.arrayDimension replicateM $
        Decoder.arrayDimension replicateM $
        Decoder.arrayDimension replicateM $
        Decoder.arrayNonNullValue $
        Decoder.int
      in
        arrayCodec (Gens.array3 Gens.auto) encoder decoder
    ,
    let
      pti =
        PTI.text
      encoder =
        Encoder.array (PTI.oidWord32 (PTI.ptiOID pti)) $
        Encoder.arrayDimension foldl' $
        Encoder.arrayDimension foldl' $
        Encoder.arrayDimension foldl' $
        Encoder.arrayValue $
        Encoder.text_strict
      decoder =
        Decoder.array $
        Decoder.arrayDimension replicateM $
        Decoder.arrayDimension replicateM $
        Decoder.arrayDimension replicateM $
        Decoder.arrayNonNullValue $
        Decoder.text_strict
      in
        arrayRoundtrip (Gens.array3 Gens.text) pti encoder decoder
  ]

textual =
  testGroup "Textual format" $
  [
    test "numeric" Gens.scientific PTI.numeric TextEncoder.numeric (const Decoder.numeric)
    ,
    test "float4" Gens.auto PTI.float4 TextEncoder.float4 (const Decoder.float4)
    ,
    test "float8" Gens.auto PTI.float8 TextEncoder.float8 (const Decoder.float8)
    ,
    test "uuid" Gens.uuid PTI.uuid TextEncoder.uuid (const Decoder.uuid)
    ,
    test "int2_int16" Gens.auto PTI.int2 TextEncoder.int2_int16 (const Decoder.int)
    ,
    test "int2_word16" Gens.postgresInt PTI.int2 TextEncoder.int2_word16 (const Decoder.int)
    ,
    test "int4_int32" Gens.auto PTI.int4 TextEncoder.int4_int32 (const Decoder.int)
    ,
    test "int4_word32" Gens.postgresInt PTI.int4 TextEncoder.int4_word32 (const Decoder.int)
    ,
    test "int8_int64" Gens.auto PTI.int8 TextEncoder.int8_int64 (const Decoder.int)
    ,
    test "int8_word64" Gens.postgresInt PTI.int8 TextEncoder.int8_word64 (const Decoder.int)
    ,
    test "bool" Gens.auto PTI.bool TextEncoder.bool (const Decoder.bool)
  ]
  where
    test typeName gen pti encoder decoder =
      QuickCheck.testProperty (typeName <> " roundtrip") $
      QuickCheck.forAll gen $ Properties.textRoundtrip (PTI.oidPQ (PTI.ptiOID pti)) encoder decoder

arrayCodec gen encoder decoder =
  QuickCheck.testProperty ("Array codec") $
  QuickCheck.forAll gen $
  \value -> (QuickCheck.===) (Right value) (Decoder.run decoder (Encoder.run encoder value))

arrayRoundtrip gen pti encoder decoder =
  QuickCheck.testProperty ("Array roundtrip") $
  QuickCheck.forAll gen $ Properties.stdRoundtrip (PTI.oidPQ (fromJust (PTI.ptiArrayOID pti))) encoder decoder

stdRoundtrip typeName gen pti encoder decoder =
  QuickCheck.testProperty (typeName <> " roundtrip") $
  QuickCheck.forAll gen $ Properties.stdRoundtrip (PTI.oidPQ (PTI.ptiOID pti)) encoder decoder

timeRoundtrip typeName gen pti encoder decoder =
  QuickCheck.testProperty (typeName <> " roundtrip") $
  QuickCheck.forAll gen $ Properties.roundtrip (PTI.oidPQ (PTI.ptiOID pti)) encoder decoder

select statement decoder value =
  HUnit.testCase (show statement) $
  HUnit.assertEqual "" (Right value) $
  unsafePerformIO $ IO.parameterlessStatement statement decoder value

    

module Main where

import Main.Prelude hiding (assert, isRight, isLeft)
import Control.Monad.IO.Class
import Test.QuickCheck.Instances
import Test.Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.SmallCheck as SmallCheck
import qualified Test.Tasty.QuickCheck as QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified PostgreSQL.Binary.Encoding as A
import qualified PostgreSQL.Binary.Decoding as B
import qualified Data.ByteString as ByteString
import qualified Main.DB as DB
import qualified Main.Gens as Gens
import qualified Main.Properties as Properties
import qualified Main.IO as IO
import qualified Main.PTI as PTI
import qualified Main.TextEncoder as TextEncoder
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Main.Apx (Apx(..))


main =
  defaultMain (testGroup "" [binary, textual])

binary =
  testGroup "Binary format" testList
  where
    testList =
      jsonb ++ other
      where
        jsonb =
          if version >= 90400
            then [primitiveRoundtrip "jsonb" Gens.aeson PTI.jsonb A.jsonb_ast B.jsonb_ast]
            else []
        other =
          [
            let
              sql =
                "select (1, 'a')"
              decoder _ =
                B.composite ((,) <$> B.valueComposite B.int <*> B.valueComposite B.char)
              expected =
                (1 :: Int64, 'a')
              in select sql decoder expected
            ,
            let
              sql =
                "select (1, null)"
              decoder _ =
                B.composite ((,) <$> B.valueComposite B.int <*> B.nullableValueComposite B.char)
              expected =
                (1 :: Int64, Nothing :: Maybe Char)
              in select sql decoder expected
            ,
            select "SELECT '1 year 2 months 3 days 4 hours 5 minutes 6 seconds 332211 microseconds' :: interval"
            (bool B.interval_float B.interval_int)
            (picosecondsToDiffTime (10^6 * (332211 + 10^6 * (6 + 60 * (5 + 60 * (4 + 24 * (3 + 31 * (2 + 12))))))))
            ,
            select "SELECT '10 seconds' :: interval"
            (bool B.interval_float B.interval_int)
            (10 :: DiffTime)
            ,
            HUnit.testCase "Interval encoder: 10 seconds" $
            let
              pti =
                PTI.interval
              encoder integerDatetimes =
                (bool A.interval_float A.interval_int integerDatetimes)
              decoder =
                (bool B.interval_float B.interval_int)
              value =
                (10 :: DiffTime)
              in
                HUnit.assertEqual "" (Right value) =<<
                IO.roundtrip (PTI.oidPQ (PTI.ptiOID pti)) encoder decoder value
            ,
            timeRoundtrip "interval" Gens.intervalDiffTime PTI.interval
            (bool A.interval_float A.interval_int)
            (bool B.interval_float B.interval_int)
            ,
            timeRoundtrip "timestamp" (fmap Apx Gens.auto) PTI.timestamp
            ((. unApx) . bool A.timestamp_float A.timestamp_int)
            (fmap Apx . bool B.timestamp_float B.timestamp_int)
            ,
            HUnit.testCase "timestamptz offset" $ do
              Right (textual, decoded) <-
                DB.session $ do
                  integerDatetimes <- DB.integerDatetimes
                  let encoder = bool A.timestamptz_float A.timestamptz_int integerDatetimes
                      decoder = bool B.timestamptz_float B.timestamptz_int integerDatetimes
                  DB.unit "DROP TABLE IF EXISTS a" []
                  DB.unit "CREATE TABLE a (b TIMESTAMPTZ)" []
                  DB.unit "set timezone to 'America/Los_Angeles'" []
                  let p = (,,) (PTI.oidPQ (PTI.ptiOID PTI.timestamptz))
                               ((A.encodingBytes . encoder) x) 
                               (LibPQ.Binary)
                      x = read "2011-09-28 00:17:25"
                  DB.unit "insert into a (b) values ($1)" [Just p]
                  DB.unit "set timezone to 'Europe/Stockholm'" []
                  textual <- DB.oneRow "SELECT * FROM a" [] LibPQ.Text
                  decoded <- fmap (B.valueParser decoder) (DB.oneRow "SELECT * FROM a" [] LibPQ.Binary)
                  return (textual, decoded)
              HUnit.assertEqual "" ("2011-09-28 02:17:25+02") textual
              HUnit.assertEqual "" (Right (read "2011-09-28 00:17:25")) decoded
            ,
            timeRoundtrip "timestamptz" (fmap Apx Gens.auto) PTI.timestamptz
            ((. unApx) . bool A.timestamptz_float A.timestamptz_int)
            (fmap Apx . bool B.timestamptz_float B.timestamptz_int)
            ,
            timeRoundtrip "timetz" (fmap Apx Gens.timetz) PTI.timetz
            ((. unApx) . bool A.timetz_float A.timetz_int)
            (fmap Apx . bool B.timetz_float B.timetz_int)
            ,
            timeRoundtrip "time" (fmap Apx Gens.auto) PTI.time
            ((. unApx) . bool A.time_float A.time_int)
            (fmap Apx . bool B.time_float B.time_int)
            ,
            primitiveRoundtrip "numeric" Gens.scientific PTI.numeric A.numeric B.numeric
            ,
            select "SELECT -1234560.789 :: numeric" (const B.numeric) (read "-1234560.789")
            ,
            select "SELECT -0.0789 :: numeric" (const B.numeric) (read "-0.0789")
            ,
            select "SELECT 10000 :: numeric" (const B.numeric) (read "10000")
            ,
            primitiveRoundtrip "float4" Gens.auto PTI.float4 A.float4 B.float4
            ,
            primitiveRoundtrip "float8" Gens.auto PTI.float8 A.float8 B.float8
            ,
            primitiveRoundtrip "char" Gens.char PTI.text A.char_utf8 B.char
            ,
            primitiveRoundtrip "text_strict" Gens.text PTI.text A.text_strict B.text_strict
            ,
            primitiveRoundtrip "text_lazy" (fmap convert Gens.text) PTI.text A.text_lazy B.text_lazy
            ,
            primitiveRoundtrip "bytea_strict" Gens.auto PTI.bytea A.bytea_strict B.bytea_strict
            ,
            primitiveRoundtrip "bytea_lazy" Gens.auto PTI.bytea A.bytea_lazy B.bytea_lazy
            ,
            primitiveRoundtrip "uuid" Gens.uuid PTI.uuid A.uuid B.uuid
            ,
            primitiveRoundtrip "inet" Gens.inet PTI.inet A.inet B.inet
            ,
            primitiveRoundtrip "int2_int16" Gens.auto PTI.int2 A.int2_int16 B.int
            ,
            primitiveRoundtrip "int2_word16" Gens.auto PTI.int2 A.int2_word16 B.int
            ,
            primitiveRoundtrip "int4_int32" Gens.auto PTI.int4 A.int4_int32 B.int
            ,
            primitiveRoundtrip "int4_word32" Gens.auto PTI.int4 A.int4_word32 B.int
            ,
            primitiveRoundtrip "int8_int64" Gens.auto PTI.int8 A.int8_int64 B.int
            ,
            primitiveRoundtrip "int8_word64" Gens.auto PTI.int8 A.int8_word64 B.int
            ,
            primitiveRoundtrip "bool" Gens.auto PTI.bool A.bool B.bool
            ,
            primitiveRoundtrip "date" Gens.auto PTI.date A.date B.date
            ,
            let
              decoder =
                B.array $
                B.dimensionArray replicateM $
                B.dimensionArray replicateM $
                B.valueArray $
                B.int
              in
                select "SELECT ARRAY[ARRAY[1,2],ARRAY[3,4]]" (const decoder) ([[1,2],[3,4]] :: [[Int]])
            ,
            let
              encoder =
                A.array (PTI.oidWord32 (PTI.ptiOID PTI.int8)) . arrayEncoder
                where
                  arrayEncoder =
                    A.dimensionArray foldl' $
                    A.dimensionArray foldl' $
                    A.dimensionArray foldl' $
                    A.encodingArray . A.int8_int64
              decoder =
                B.array $
                B.dimensionArray replicateM $
                B.dimensionArray replicateM $
                B.dimensionArray replicateM $
                B.valueArray $
                B.int
              in
                arrayCodec (Gens.array3 Gens.auto) encoder decoder
            ,
            let
              pti =
                PTI.text
              encoder =
                A.array (PTI.oidWord32 (PTI.ptiOID pti)) . arrayEncoder
                where
                  arrayEncoder =
                    A.dimensionArray foldl' $
                    A.dimensionArray foldl' $
                    A.dimensionArray foldl' $
                    A.encodingArray . A.text_strict
              decoder =
                B.array $
                B.dimensionArray replicateM $
                B.dimensionArray replicateM $
                B.dimensionArray replicateM $
                B.valueArray $
                B.text_strict
              in
                arrayRoundtrip (Gens.array3 Gens.text) pti encoder decoder
          ]

textual =
  testGroup "Textual format" $
  [
    test "numeric" Gens.scientific PTI.numeric TextEncoder.numeric (const B.numeric)
    ,
    test "float4" Gens.auto PTI.float4 TextEncoder.float4 (const B.float4)
    ,
    test "float8" Gens.auto PTI.float8 TextEncoder.float8 (const B.float8)
    ,
    test "uuid" Gens.uuid PTI.uuid TextEncoder.uuid (const B.uuid)
    ,
    test "int2_int16" Gens.auto PTI.int2 TextEncoder.int2_int16 (const B.int)
    ,
    test "int2_word16" Gens.postgresInt PTI.int2 TextEncoder.int2_word16 (const B.int)
    ,
    test "int4_int32" Gens.auto PTI.int4 TextEncoder.int4_int32 (const B.int)
    ,
    test "int4_word32" Gens.postgresInt PTI.int4 TextEncoder.int4_word32 (const B.int)
    ,
    test "int8_int64" Gens.auto PTI.int8 TextEncoder.int8_int64 (const B.int)
    ,
    test "int8_word64" Gens.postgresInt PTI.int8 TextEncoder.int8_word64 (const B.int)
    ,
    test "bool" Gens.auto PTI.bool TextEncoder.bool (const B.bool)
  ]
  where
    test typeName gen pti encoder decoder =
      QuickCheck.testProperty (typeName <> " roundtrip") $
      QuickCheck.forAll gen $ Properties.textRoundtrip (PTI.oidPQ (PTI.ptiOID pti)) encoder decoder

arrayCodec gen encoder decoder =
  QuickCheck.testProperty ("Array codec") $
  QuickCheck.forAll gen $
  \value -> (QuickCheck.===) (Right value) (B.valueParser decoder ((A.encodingBytes . encoder) value))

arrayRoundtrip gen pti encoder decoder =
  QuickCheck.testProperty ("Array roundtrip") $
  QuickCheck.forAll gen $ Properties.stdRoundtrip (PTI.oidPQ (fromJust (PTI.ptiArrayOID pti))) encoder decoder

stdRoundtrip typeName gen pti encoder decoder =
  QuickCheck.testProperty (typeName <> " roundtrip") $
  QuickCheck.forAll gen $ Properties.stdRoundtrip (PTI.oidPQ (PTI.ptiOID pti)) encoder decoder

primitiveRoundtrip typeName gen pti encoder decoder =
  stdRoundtrip typeName gen pti (encoder) decoder

timeRoundtrip typeName gen pti encoder decoder =
  QuickCheck.testProperty (typeName <> " roundtrip") $
  QuickCheck.forAll gen $ Properties.roundtrip (PTI.oidPQ (PTI.ptiOID pti)) (\x -> encoder x) decoder

select statement decoder value =
  HUnit.testCase (show statement) $
  HUnit.assertEqual "" (Right value) $
  unsafePerformIO $ IO.parameterlessStatement statement decoder value

{-# NOINLINE version #-}
version :: Int
version =
  either (error . show) id $
  unsafePerformIO $
  DB.session $
  DB.serverVersion    

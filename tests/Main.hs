{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import BasePrelude hiding (assert)
import Test.Framework
import Test.QuickCheck.Instances
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQLBinary.PTI as PTI
import qualified PostgreSQLBinary.Encoder as Encoder
import qualified PostgreSQLBinary.Decoder as Decoder
import qualified PostgreSQLBinary.Array as Array


type Text = T.Text
type LazyText = TL.Text
type ByteString = B.ByteString
type LazyByteString = BL.ByteString
type Scientific = Scientific.Scientific

main = 
  htfMain $ htf_thisModulesTests

floatEqProp :: RealFrac a => Show a => a -> a -> Property
floatEqProp a b =
  counterexample (show a ++ " /~ " ++ show b) $
    a + error >= b && a - error <= b
  where
    error = max (abs a) 1 / 10^3

mappingP :: 
  (Show a, Eq a) => 
  Word32 -> (a -> Maybe ByteString) -> (Maybe ByteString -> Either Text a) -> a -> Property
mappingP oid encode decode v =
  Right v === do
    unsafePerformIO $ do
      c <- connect
      initConnection c
      Just result <-
        let param = (,,) <$> pure (PQ.Oid $ fromIntegral oid) <*> encode v <*> pure PQ.Binary
            in PQ.execParams c "SELECT $1" [param] PQ.Binary
      binaryResult <- PQ.getvalue result 0 0
      PQ.finish c
      return $ decode binaryResult

mappingTextP ::
  (Show a, Eq a) => 
  Word32 -> (a -> Maybe ByteString) -> (a -> Maybe ByteString) -> a -> Property
mappingTextP oid encode render value =
  render value === do unsafePerformIO $ checkText oid (encode value)

checkText :: Word32 -> Maybe ByteString -> IO (Maybe ByteString)
checkText oid v =
  do
    c <- connect
    initConnection c
    Just result <-
      let param = (,,) <$> pure (PQ.Oid $ fromIntegral oid) <*> v <*> pure PQ.Binary
          in PQ.execParams c "SELECT $1" [param] PQ.Text
    encodedResult <- PQ.getvalue result 0 0
    PQ.finish c
    return $ encodedResult

query :: ByteString -> [Maybe (PQ.Oid, ByteString, PQ.Format)] -> PQ.Format -> IO (Maybe ByteString)
query statement params outFormat =
  do
    connection <- connect
    initConnection connection
    Just result <- PQ.execParams connection statement params outFormat
    encodedResult <- PQ.getvalue result 0 0
    PQ.finish connection
    return $ encodedResult

connect :: IO PQ.Connection
connect =
  PQ.connectdb bs
  where
    bs = 
      B.intercalate " " components
      where
        components = 
          [
            "host=" <> host,
            "port=" <> (fromString . show) port,
            "user=" <> user,
            "password=" <> password,
            "dbname=" <> db
          ]
          where
            host = "localhost"
            port = 5432
            user = "postgres"
            password = ""
            db = "postgres"

initConnection :: PQ.Connection -> IO ()
initConnection c =
  void $ PQ.exec c $ mconcat $ map (<> ";") $ 
    [ "SET standard_conforming_strings TO on",
      "SET datestyle TO ISO",
      "SET client_encoding = 'UTF8'",
      "SET client_min_messages TO WARNING",
      "SET bytea_output = 'hex'" ]

nonNullParser p =
  fromMaybe (Left "Unexpected NULL") . fmap p

nonNullRenderer r =
  return . r

-- * Generators
-------------------------

scientificGen :: Gen Scientific
scientificGen =
  Scientific.scientific <$> arbitrary <*> arbitrary

microsTimeOfDayGen :: Gen TimeOfDay
microsTimeOfDayGen =
  timeToTimeOfDay <$> microsDiffTimeGen

microsLocalTimeGen :: Gen LocalTime
microsLocalTimeGen = 
  LocalTime <$> arbitrary <*> microsTimeOfDayGen

microsDiffTimeGen :: Gen DiffTime
microsDiffTimeGen = do
  fmap picosecondsToDiffTime $ fmap (* (10^6)) $ choose (0, (10^6)*24*60*60)

timeZoneGen :: Gen TimeZone
timeZoneGen =
  minutesToTimeZone <$> choose (- 60 * 12 + 1, 60 * 12)

arrayGen :: Gen (Word32, Array.Data)
arrayGen =
  do
    ndims <- choose (1, 4)
    dims <- replicateM ndims dimGen
    (valueGen', oid, arrayOID) <- valueGen
    values <- replicateM (dimsToNValues dims) valueGen'
    let nulls = elem Nothing values
    return (arrayOID, (dims, values, nulls, oid))
  where
    dimGen =
      (,) <$> choose (1, 7) <*> pure 1
    valueGen =
      do
        (pti, gen) <- elements [(PTI.int8, mkGen (Encoder.int8 . Left)),
                                (PTI.bool, mkGen Encoder.bool),
                                (PTI.date, mkGen Encoder.date),
                                (PTI.text, mkGen Encoder.text),
                                (PTI.bytea, mkGen Encoder.bytea)]
        return (gen, PTI.oidOf pti, fromJust $ PTI.arrayOIDOf pti)
      where
        mkGen renderer =
          fmap (fmap renderer) arbitrary
    dimsToNValues =
      product . map dimensionWidth
      where
        dimensionWidth (x, _) = fromIntegral x


-- * Properties
-------------------------

prop_timestamp =
  forAll microsLocalTimeGen $ 
    mappingP (PTI.oidOf PTI.timestamp) 
             (nonNullRenderer Encoder.timestamp)
             (nonNullParser Decoder.timestamp)

test_timestampParsing1 =
  assertEqual (Right (read "2000-01-19 10:41:06" :: LocalTime)) =<< do
    fmap (Decoder.timestamp . fromJust) $ 
      query "SELECT '2000-01-19 10:41:06' :: timestamp" [] PQ.Binary

test_timestampParsing2 =
  assertEqual (Right (read "1864-05-09 22:48:34.254242" :: LocalTime)) =<< do
    fmap (Decoder.timestamp . fromJust) $ 
      query "SELECT '1864-05-09 22:48:34.254242' :: timestamp" [] PQ.Binary

prop_timetz =
  forAll ((,) <$> microsTimeOfDayGen <*> timeZoneGen) $ 
    mappingP (PTI.oidOf PTI.timetz) 
             (nonNullRenderer Encoder.timetz)
             (nonNullParser Decoder.timetz)

test_timetzParsing1 =
  assertEqual (Right (read "(10:41:06.002897, +0500)" :: (TimeOfDay, TimeZone))) =<< do
    fmap (Decoder.timetz . fromJust) $ 
      query "SELECT '10:41:06.002897+05' :: timetz" [] PQ.Binary

prop_timeOfDay =
  forAll microsTimeOfDayGen $ 
    mappingP (PTI.oidOf PTI.time) 
             (nonNullRenderer Encoder.time)
             (nonNullParser Decoder.time)

prop_timeOfDayParsing =
  forAll microsTimeOfDayGen $ \x ->
    Right x === do
      unsafePerformIO $ 
        fmap (Decoder.time . fromJust) $ 
          query "SELECT $1" 
                [Just (PQ.Oid $ fromIntegral $ PTI.oidOf PTI.time, (fromString . show) x, PQ.Text)] 
                PQ.Binary

prop_scientific (c, e) =
  let x = Scientific.scientific c e
    in
      mappingP (PTI.oidOf PTI.numeric) 
               (nonNullRenderer Encoder.numeric)
               (nonNullParser Decoder.numeric)
               (x)

test_scientificParsing1 =
  assertEqual (Right (read "-1234560.789" :: Scientific)) =<< do
    fmap (Decoder.numeric . fromJust) $ 
      query "SELECT -1234560.789 :: numeric" [] PQ.Binary

test_scientificParsing2 =
  assertEqual (Right (read "-0.0789" :: Scientific)) =<< do
    fmap (Decoder.numeric . fromJust) $ 
      query "SELECT -0.0789 :: numeric" [] PQ.Binary

test_scientificParsing3 =
  assertEqual (Right (read "10000" :: Scientific)) =<< do
    fmap (Decoder.numeric . fromJust) $ 
      query "SELECT 10000 :: numeric" [] PQ.Binary

prop_scientificParsing (c, e) =
  let x = Scientific.scientific c e
    in
      Right x === do
        unsafePerformIO $ 
          fmap (Decoder.numeric . fromJust) $ 
            query "SELECT $1 :: numeric" 
                  [Just (PQ.Oid $ fromIntegral $ PTI.oidOf PTI.numeric, (fromString . show) x, PQ.Text)] 
                  PQ.Binary

prop_float =
  mappingP (PTI.oidOf PTI.float4) 
           (nonNullRenderer Encoder.float4)
           (nonNullParser Decoder.float4)

prop_floatText =
  \x -> 
    floatEqProp x $ do
      fromJust $ unsafePerformIO $ (fmap . fmap) reader $ checkText (PTI.oidOf pti) (encoder x)
  where
    pti = PTI.float4
    reader = read . BC.unpack
    encoder = nonNullRenderer Encoder.float4

prop_double =
  mappingP (PTI.oidOf PTI.float8) 
           (nonNullRenderer Encoder.float8)
           (nonNullParser Decoder.float8)

prop_doubleText =
  \x -> 
    floatEqProp x $ do
      fromJust $ unsafePerformIO $ (fmap . fmap) reader $ checkText (PTI.oidOf pti) (encoder x)
  where
    pti = PTI.float8
    reader = read . BC.unpack
    encoder = nonNullRenderer Encoder.float8

prop_char x =
  (x /= '\NUL') ==>
  mappingP (PTI.oidOf PTI.text) 
           (nonNullRenderer Encoder.char)
           (nonNullParser Decoder.char)
           (x)

prop_charText x =
  (x /= '\NUL') ==>
  mappingTextP (PTI.oidOf PTI.text) 
               (nonNullRenderer Encoder.char) 
               (Just . TE.encodeUtf8 . T.singleton)
               (x)

test_emptyArrayElements =
  assertEqual [] (Array.elements ([], [], False, 0))

test_arrayElements =
  assertEqual result (Array.elements arrayData)
  where
    arrayData = ([(3, 1)], [Just "1", Just "2", Just "3"], False, 0)
    result = [([], [Just "1"], False, 0), ([], [Just "2"], False, 0), ([], [Just "3"], False, 0)]

prop_arrayDataFromAndToListIsomporphism =
  forAll arrayGen $ \(oid, x) ->
    x === (Array.fromListUnsafe . Array.elements) x

prop_byteString =
  mappingP (PTI.oidOf PTI.bytea)
           (nonNullRenderer (Encoder.bytea . Left))
           (nonNullParser Decoder.bytea)

prop_lazyByteString =
  mappingP (PTI.oidOf PTI.bytea)
           (nonNullRenderer (Encoder.bytea . Right))
           (nonNullParser (fmap BL.fromStrict . Decoder.bytea))

prop_text v =
  (isNothing $ T.find (== '\NUL') v) ==>
    mappingP (PTI.oidOf PTI.text) 
             (nonNullRenderer (Encoder.text . Left))
             (nonNullParser Decoder.text)
             (v)

prop_lazyText v =
  (isNothing $ TL.find (== '\NUL') v) ==>
    mappingP (PTI.oidOf PTI.text) 
             (nonNullRenderer (Encoder.text . Right))
             (nonNullParser (fmap TL.fromStrict . Decoder.text))
             (v)

prop_bool =
  mappingP (PTI.oidOf PTI.bool) 
           (nonNullRenderer Encoder.bool)
           (nonNullParser Decoder.bool)

prop_int =
  mappingP (PTI.oidOf PTI.int8) 
           (nonNullRenderer (Encoder.int8 . Left) . (fromIntegral :: Int -> Int64))
           (nonNullParser Decoder.int)

prop_int8 =
  mappingP (PTI.oidOf PTI.int2) 
           (nonNullRenderer (Encoder.int2 . Left) . (fromIntegral :: Int8 -> Int16))
           (nonNullParser Decoder.int)

prop_int16 =
  mappingP (PTI.oidOf PTI.int2) 
           (nonNullRenderer (Encoder.int2 . Left))
           (nonNullParser Decoder.int)

prop_int32 =
  mappingP (PTI.oidOf PTI.int4) 
           (nonNullRenderer (Encoder.int4 . Left))
           (nonNullParser Decoder.int)

prop_int64 =
  mappingP (PTI.oidOf PTI.int8) 
           (nonNullRenderer (Encoder.int8 . Left))
           (nonNullParser Decoder.int)

prop_int64Text =
  mappingTextP (PTI.oidOf PTI.int8) 
               (nonNullRenderer (Encoder.int8 . Left)) 
               (Just . fromString . show)

prop_word =
  mappingP (PTI.oidOf PTI.int8) 
           (nonNullRenderer (Encoder.int8 . Right) . (fromIntegral :: Word -> Word64))
           (nonNullParser Decoder.int)

prop_word8 =
  mappingP (PTI.oidOf PTI.int2) 
           (nonNullRenderer (Encoder.int2 . Right) . (fromIntegral :: Word8 -> Word16))
           (nonNullParser Decoder.int)

prop_word16 =
  mappingP (PTI.oidOf PTI.int2) 
           (nonNullRenderer (Encoder.int2 . Right))
           (nonNullParser Decoder.int)

prop_word32 =
  mappingP (PTI.oidOf PTI.int4) 
           (nonNullRenderer (Encoder.int4 . Right))
           (nonNullParser Decoder.int)

prop_word64 =
  mappingP (PTI.oidOf PTI.int8) 
           (nonNullRenderer (Encoder.int8 . Right))
           (nonNullParser Decoder.int)

prop_word64Text =
  mappingTextP (PTI.oidOf PTI.int8) 
               (nonNullRenderer (Encoder.int8 . Right)) 
               (Just . fromString . show)

prop_day =
  mappingP (PTI.oidOf PTI.date) 
           (nonNullRenderer Encoder.date)
           (nonNullParser Decoder.date)

prop_dayText =
  mappingTextP (PTI.oidOf PTI.date) 
               (nonNullRenderer Encoder.date) 
               (Just . fromString . show)

prop_arrayData =
  forAll arrayGen $ uncurry $ \oid ->
    mappingP (oid)
             (nonNullRenderer Encoder.array)
             (nonNullParser Decoder.array)


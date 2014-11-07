{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import BasePrelude hiding (assert)
import Test.Framework
import Test.QuickCheck.Instances
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified PostgreSQLBinary.PTI as PTI
import qualified PostgreSQLBinary.Rendering as Rendering
import qualified PostgreSQLBinary.Parsing as Parsing
import qualified PostgreSQLBinary.Array as Array


type Text = T.Text
type LazyText = TL.Text
type ByteString = B.ByteString
type LazyByteString = BL.ByteString
type Scientific = Scientific.Scientific

main = 
  htfMain $ htf_thisModulesTests

mappingP :: 
  (Show a, Eq a, Arbitrary a) => 
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
  (Show a, Eq a, Arbitrary a) => 
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

-- * Properties
-------------------------

prop_text v =
  (isNothing $ T.find (== '\NUL') v) ==>
    mappingP (PTI.oidOf PTI.text) 
             (nonNullRenderer Rendering.text)
             (nonNullParser Parsing.text)
             (v)

prop_bool =
  mappingP (PTI.oidOf PTI.bool) 
           (nonNullRenderer Rendering.bool)
           (nonNullParser Parsing.bool)

prop_int =
  mappingP (PTI.oidOf PTI.int8) 
           (nonNullRenderer Rendering.int64 . (fromIntegral :: Int -> Int64))
           (nonNullParser Parsing.integral)

prop_int8 =
  mappingP (PTI.oidOf PTI.int2) 
           (nonNullRenderer Rendering.int16 . (fromIntegral :: Int8 -> Int16))
           (nonNullParser Parsing.integral)

prop_int16 =
  mappingP (PTI.oidOf PTI.int2) 
           (nonNullRenderer Rendering.int16)
           (nonNullParser Parsing.integral)

prop_int32 =
  mappingP (PTI.oidOf PTI.int4) 
           (nonNullRenderer Rendering.int32)
           (nonNullParser Parsing.integral)

prop_int64 =
  mappingP (PTI.oidOf PTI.int8) 
           (nonNullRenderer Rendering.int64)
           (nonNullParser Parsing.integral)

prop_word =
  mappingP (PTI.oidOf PTI.int8) 
           (nonNullRenderer Rendering.word64 . (fromIntegral :: Word -> Word64))
           (nonNullParser Parsing.integral)

prop_word8 =
  mappingP (PTI.oidOf PTI.int2) 
           (nonNullRenderer Rendering.word16 . (fromIntegral :: Word8 -> Word16))
           (nonNullParser Parsing.integral)

prop_word16 =
  mappingP (PTI.oidOf PTI.int2) 
           (nonNullRenderer Rendering.word16)
           (nonNullParser Parsing.integral)

prop_word32 =
  mappingP (PTI.oidOf PTI.int4) 
           (nonNullRenderer Rendering.word32)
           (nonNullParser Parsing.integral)

prop_word64 =
  mappingP (PTI.oidOf PTI.int8) 
           (nonNullRenderer Rendering.word64)
           (nonNullParser Parsing.integral)

prop_day =
  mappingP (PTI.oidOf PTI.date) 
           (nonNullRenderer Rendering.day)
           (nonNullParser Parsing.day)

prop_dayText =
  mappingTextP (PTI.oidOf PTI.date) 
               (nonNullRenderer Rendering.day) 
               (Just . fromString . show)

prop_arrayData =
  forAll arrayDataGen $ uncurry $ \oid ->
    mappingP (oid)
             (nonNullRenderer Rendering.arrayData)
             (nonNullParser Parsing.arrayData)

-- * Gens
-------------------------

arrayDataGen :: Gen (Word32, Array.Data)
arrayDataGen =
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
        (pti, gen) <- elements [(PTI.int8, mkGen Rendering.int64),
                                (PTI.bool, mkGen Rendering.bool),
                                (PTI.date, mkGen Rendering.day)]
        return (gen, PTI.oidOf pti, fromJust $ PTI.arrayOIDOf pti)
      where
        mkGen renderer =
          fmap (fmap renderer) arbitrary
    dimsToNValues =
      product . map dimensionWidth
      where
        dimensionWidth (x, _) = fromIntegral x



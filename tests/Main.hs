{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import BasePrelude hiding (assert)
import Test.Framework
import Test.QuickCheck.Instances
import Data.Time
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified LibpqBinary.PTI as PTI
import qualified LibpqBinary.Rendering as Rendering
import qualified LibpqBinary.Parsing as Parsing


type Text = Data.Text.Text
type LazyText = Data.Text.Lazy.Text
type ByteString = B.ByteString
type LazyByteString = BL.ByteString
type Scientific = Scientific.Scientific

main = 
  htfMain $ htf_thisModulesTests

mappingP :: 
  (Show a, Eq a, Arbitrary a) => 
  (PQ.Oid, a -> Maybe ByteString, Maybe ByteString -> Either Text a) -> a -> Property
mappingP (oid, encode, decode) v =
  counterexample ("Encoded: " <> show (encode v)) $
  Right v === do
    unsafePerformIO $ do
      c <- connect
      initConnection c
      Just result <-
        let param = (,,) <$> pure oid <*> encode v <*> pure PQ.Binary
            in PQ.execParams c "SELECT $1" [param] PQ.Binary
      binaryResult <- PQ.getvalue result 0 0
      PQ.finish c
      return $ decode binaryResult
  where
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
    initConnection c =
      PQ.exec c $ mconcat $ map (<> ";") $ 
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

prop_bool =
  mappingP ((PTI.oidOf PTI.bool), nonNullRenderer Rendering.bool, nonNullParser Parsing.bool)

prop_int =
  mappingP ((PTI.oidOf PTI.int8), nonNullRenderer Rendering.int, nonNullParser Parsing.integral)

prop_int64 =
  mappingP ((PTI.oidOf PTI.int8), nonNullRenderer Rendering.int64, nonNullParser Parsing.integral)


-- |
-- Encoders to text format.
module Main.TextEncoder  where

import Main.Prelude hiding (maybe, bool)
import Data.ByteString.Builder
import qualified Data.ByteString.Builder.Scientific
import qualified Data.Text.Encoding
import qualified Data.UUID
import qualified Main.Prelude as Prelude

type Encoder a =
  a -> Builder

bool :: Encoder Bool
bool =
  byteString . Prelude.bool "false" "true"

int2_int16 :: Encoder Int16
int2_int16 =
  int16Dec
 
int4_int32 :: Encoder Int32
int4_int32 =
  int32Dec
 
int8_int64 :: Encoder Int64
int8_int64 =
  int64Dec

int2_word16 :: Encoder Word16
int2_word16 =
  word16Dec
 
int4_word32 :: Encoder Word32
int4_word32 =
  word32Dec
 
int8_word64 :: Encoder Word64
int8_word64 =
  word64Dec

float4 :: Encoder Float
float4 =
  floatDec

float8 :: Encoder Double
float8 =
  doubleDec

numeric :: Encoder Scientific
numeric =
  Data.ByteString.Builder.Scientific.scientificBuilder

uuid :: Encoder UUID
uuid =
  byteString . Data.UUID.toASCIIBytes

bytea_strict :: Encoder ByteString
bytea_strict =
  byteString


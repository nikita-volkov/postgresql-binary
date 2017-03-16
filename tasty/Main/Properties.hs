module Main.Properties where

import Main.Prelude hiding (assert, isRight, isLeft)
import Test.QuickCheck
import Test.QuickCheck.Instances
import qualified Data.Scientific as Scientific
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import qualified PostgreSQL.Binary.Encoding as Encoder
import qualified PostgreSQL.Binary.Decoder as Decoder
import qualified Main.TextEncoder  as TextEncoder 
import qualified Main.PTI as PTI
import qualified Main.DB as DB
import qualified Main.IO as IO
import qualified Database.PostgreSQL.LibPQ as LibPQ


roundtrip :: (Show a, Eq a) => 
  LibPQ.Oid -> (Bool -> (a -> Encoder.Value)) -> (Bool -> Decoder.Decoder a) -> a -> Property
roundtrip oid encoder decoder value =
  Right value === unsafePerformIO (IO.roundtrip oid encoder decoder value)

stdRoundtrip :: (Show a, Eq a) => LibPQ.Oid -> (a -> Encoder.Value) -> Decoder.Decoder a -> a -> Property
stdRoundtrip oid encoder decoder value =
  roundtrip oid (const encoder) (const decoder) value

textRoundtrip :: (Show a, Eq a) => LibPQ.Oid -> TextEncoder.Encoder a -> (Bool -> Decoder.Decoder a) -> a -> Property
textRoundtrip oid encoder decoder value =
  Right value === unsafePerformIO (IO.textRoundtrip oid encoder decoder value)

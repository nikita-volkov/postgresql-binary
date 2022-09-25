module Main.Properties where

import qualified Data.Scientific as Scientific
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Main.DB as DB
import qualified Main.IO as IO
import qualified Main.PTI as PTI
import Main.Prelude hiding (assert, isLeft, isRight)
import qualified Main.TextEncoder as C
import qualified PostgreSQL.Binary.Decoding as A
import qualified PostgreSQL.Binary.Encoding as B
import Test.QuickCheck
import Test.QuickCheck.Instances

roundtrip ::
  (Show a, Eq a) =>
  LibPQ.Oid ->
  (Bool -> (a -> B.Encoding)) ->
  (Bool -> A.Value a) ->
  a ->
  Property
roundtrip oid encoder decoder value =
  Right value === unsafePerformIO (IO.roundtrip oid encoder decoder value)

stdRoundtrip :: (Show a, Eq a) => LibPQ.Oid -> (a -> B.Encoding) -> A.Value a -> a -> Property
stdRoundtrip oid encoder decoder value =
  roundtrip oid (const encoder) (const decoder) value

textRoundtrip :: (Show a, Eq a) => LibPQ.Oid -> C.Encoder a -> (Bool -> A.Value a) -> a -> Property
textRoundtrip oid encoder decoder value =
  Right value === unsafePerformIO (IO.textRoundtrip oid encoder decoder value)

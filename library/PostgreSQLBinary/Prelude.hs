module PostgreSQLBinary.Prelude
( 
  module Exports,
  LazyByteString,
  LazyText,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports

-- text
-------------------------
import Data.Text as Exports (Text)

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- scientific
-------------------------
import Data.Scientific as Exports (Scientific)

-- custom
-------------------------
import qualified Data.Text.Lazy
import qualified Data.ByteString.Lazy


type LazyByteString = Data.ByteString.Lazy.ByteString
type LazyText = Data.Text.Lazy.Text

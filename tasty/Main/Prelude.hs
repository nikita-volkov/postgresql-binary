module Main.Prelude
( 
  module Exports,
  LazyByteString,
  ByteStringBuilder,
  LazyText,
  TextBuilder,
)
where


-- rerebase
-------------------------
import Prelude as Exports hiding (assert, Data, fail, check)

-- conversion
-------------------------
import Conversion as Exports
import Conversion.Text ()
import Conversion.ByteString ()

-- custom
-------------------------
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Builder
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder


type LazyByteString =
  Data.ByteString.Lazy.ByteString

type ByteStringBuilder =
  Data.ByteString.Builder.Builder

type LazyText =
  Data.Text.Lazy.Text

type TextBuilder =
  Data.Text.Lazy.Builder.Builder


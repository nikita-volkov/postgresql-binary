module Main.Prelude
( 
  module Exports,
  LazyByteString,
  ByteStringBuilder,
  LazyText,
  TextBuilder,
  bug,
  bottom,
)
where


-- rebase
-------------------------
import Rebase.Prelude as Exports hiding (assert, Data, fail)

-- conversion
-------------------------
import Conversion as Exports
import Conversion.Text ()
import Conversion.ByteString ()

-- placeholders
-------------------------
import Development.Placeholders as Exports

-- loch-th
-------------------------
import Debug.Trace.LocationTH as Exports

-- custom
-------------------------
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Builder
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Debug.Trace.LocationTH


type LazyByteString =
  Data.ByteString.Lazy.ByteString

type ByteStringBuilder =
  Data.ByteString.Builder.Builder

type LazyText =
  Data.Text.Lazy.Text

type TextBuilder =
  Data.Text.Lazy.Builder.Builder


bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"postgresql-binary\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]

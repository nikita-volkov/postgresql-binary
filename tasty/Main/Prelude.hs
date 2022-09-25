module Main.Prelude
  ( module Exports,
    LazyByteString,
    ByteStringBuilder,
    LazyText,
    TextBuilder,
  )
where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import Prelude as Exports hiding (Data, assert, check, fail)

type LazyByteString =
  Data.ByteString.Lazy.ByteString

type ByteStringBuilder =
  Data.ByteString.Builder.Builder

type LazyText =
  Data.Text.Lazy.Text

type TextBuilder =
  Data.Text.Lazy.Builder.Builder

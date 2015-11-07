module PostgreSQL.Binary.Prelude
( 
  module Exports,
  LazyByteString,
  ByteStringBuilder,
  LazyText,
  TextBuilder,
  bug,
  bottom,
  mapLeft,
  joinMap,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (assert, Data, fail)

-- transformers
-------------------------
import Control.Monad.Trans.State.Strict as Exports hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.Reader as Exports hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.Class as Exports
import Data.Functor.Identity as Exports

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- text
-------------------------
import Data.Text as Exports (Text)

-- vector
-------------------------
import Data.Vector as Exports (Vector)

-- scientific
-------------------------
import Data.Scientific as Exports (Scientific)

-- uuid
-------------------------
import Data.UUID as Exports (UUID)

-- time
-------------------------
import Data.Time as Exports

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

{-# INLINE mapLeft #-}
mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft f =
  either (Left . f) Right

joinMap :: Monad m => (a -> m b) -> m a -> m b
joinMap f =
  join . fmap f

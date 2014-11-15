module PostgreSQLBinary.Prelude
( 
  module Exports,
  LazyByteString,
  LazyText,
  bug,
  bottom,
  partial,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports

-- transformers
-------------------------
import Control.Monad.Trans.State.Strict as Exports hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.Reader as Exports hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.Class as Exports

-- text
-------------------------
import Data.Text as Exports (Text)

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

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

-- custom
-------------------------
import qualified Debug.Trace.LocationTH
import qualified Data.Text.Lazy
import qualified Data.ByteString.Lazy


type LazyByteString = Data.ByteString.Lazy.ByteString
type LazyText = Data.Text.Lazy.Text

bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"postgresql-binary\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]

partial :: Alternative f => (a -> Bool) -> a -> f a
partial p x = 
  if p x then pure x else empty

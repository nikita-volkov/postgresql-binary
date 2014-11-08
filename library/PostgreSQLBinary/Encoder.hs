module PostgreSQLBinary.Encoder where

import PostgreSQLBinary.Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Builder.Scientific as Scientific
import qualified PostgreSQLBinary.Encoder.Builder as Builder
import qualified PostgreSQLBinary.Array as Array
import qualified PostgreSQLBinary.Date as Date
import qualified PostgreSQLBinary.Numeric as Numeric


-- |
-- A function for rendering a value into a byte string.
type E a = a -> ByteString


-- * Numbers
-------------------------

int2 :: E (Either Int16 Word16)
int2 = 
  Builder.run . either BB.int16BE BB.word16BE

int4 :: E (Either Int32 Word32)
int4 = 
  Builder.run . either BB.int32BE BB.word32BE

int8 :: E (Either Int64 Word64)
int8 = 
  Builder.run . either BB.int64BE BB.word64BE

float4 :: E Float
float4 =
  Builder.run . BB.word32BE . unsafeCoerce

float8 :: E Double
float8 =
  Builder.run . BB.word64BE . unsafeCoerce

numeric :: E Scientific
numeric =
  (. Numeric.fromScientific) $ \x ->
    Builder.run $
      BB.word16BE (Numeric.componentsAmount x) <>
      BB.int16BE (Numeric.pointIndex x) <>
      BB.word16BE (Numeric.signCode x) <>
      BB.word16BE (Numeric.amountOfDigitsAfterPoint x) <>
      foldMap BB.word16BE (Numeric.components x)

-- * Text
-------------------------

-- |
-- A UTF-8-encoded char.
-- 
-- Note that since it's UTF-8-encoded
-- not a \"char\" but a \"text\" OID should be used with it.
char :: E Char
char = 
  text . Left . T.singleton

-- |
-- Either a strict or a lazy text.
text :: E (Either Text TL.Text)
text =
  either strict lazy
  where
    strict = TE.encodeUtf8 . T.filter (/= '\0')
    lazy = BL.toStrict . TLE.encodeUtf8 . TL.filter (/= '\0')

-- |
-- Either a strict or a lazy bytestring.
bytea :: E (Either ByteString BL.ByteString)
bytea =
  either id BL.toStrict

-- * Date and Time
-------------------------

date :: E Day
date =
  Builder.run . Builder.date

time :: E TimeOfDay
time =
  Builder.run . Builder.time

timetz :: E (TimeOfDay, TimeZone)
timetz =
  Builder.run . Builder.timetz

timestamp :: E UTCTime
timestamp =
  Builder.run . Builder.timestamp

timestamptz :: E LocalTime
timestamptz =
  Builder.run . Builder.timestamptz


-- * Misc
-------------------------

bool :: E Bool
bool =
  \case
    False -> B.singleton 0
    True  -> B.singleton 1

array :: E Array.Data
array = 
  Builder.run . Builder.array

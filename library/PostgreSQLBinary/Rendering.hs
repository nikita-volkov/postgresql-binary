module PostgreSQLBinary.Rendering where

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
import qualified PostgreSQLBinary.Rendering.Builder as Builder
import qualified PostgreSQLBinary.Array as Array
import qualified PostgreSQLBinary.Date as Date
import qualified PostgreSQLBinary.Numeric as Numeric


-- |
-- A function for rendering a value into a byte string.
type R a = a -> ByteString


-- * Numbers
-------------------------

int2 :: R (Either Int16 Word16)
int2 = 
  Builder.run . either BB.int16BE BB.word16BE

int4 :: R (Either Int32 Word32)
int4 = 
  Builder.run . either BB.int32BE BB.word32BE

int8 :: R (Either Int64 Word64)
int8 = 
  Builder.run . either BB.int64BE BB.word64BE

float4 :: R Float
float4 =
  Builder.run . BB.word32BE . unsafeCoerce

float8 :: R Double
float8 =
  Builder.run . BB.word64BE . unsafeCoerce

numeric :: R Scientific
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
char :: R Char
char = 
  text . Left . T.singleton

-- |
-- Either a strict or a lazy text.
text :: R (Either Text TL.Text)
text =
  either strict lazy
  where
    strict = TE.encodeUtf8 . T.filter (/= '\0')
    lazy = BL.toStrict . TLE.encodeUtf8 . TL.filter (/= '\0')

-- |
-- Either a strict or a lazy bytestring.
bytea :: R (Either ByteString BL.ByteString)
bytea =
  either id BL.toStrict

-- * Date and Time
-------------------------

date :: R Day
date =
  Builder.run . BB.int32BE . fromIntegral . Date.dayToPostgresJulian

time :: R TimeOfDay
time =
  Builder.run . BB.word64BE . (`div` (10^6)) . unsafeCoerce timeOfDayToTime

-- * Misc
-------------------------

bool :: R Bool
bool =
  \case
    False -> B.singleton 0
    True  -> B.singleton 1

array :: R Array.Data
array = 
  Builder.run . Builder.array

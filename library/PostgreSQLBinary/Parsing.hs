module PostgreSQLBinary.Parsing where

import PostgreSQLBinary.Prelude hiding (bool)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified PostgreSQLBinary.Parsing.Atto as Atto
import qualified PostgreSQLBinary.Array as Array
import qualified PostgreSQLBinary.Date as Date
import qualified PostgreSQLBinary.Integral as Integral
import qualified PostgreSQLBinary.Numeric as Numeric


-- |
-- A function for decoding a byte string into a value.
type P a = ByteString -> Either Text a


-- * Numbers
-------------------------

-- |
-- Any of PostgreSQL integer types.
{-# INLINE int #-}
int :: (Integral a, Bits a) => P a
int =
  Right . Integral.pack

float4 :: P Float
float4 =
  unsafeCoerce . (int :: P Word32)

float8 :: P Double
float8 =
  unsafeCoerce . (int :: P Word64)

numeric :: P Scientific
numeric =
  join . fmap Numeric.toScientific . flip Atto.run Atto.numeric

-- * Text
-------------------------

-- |
-- A UTF-8-encoded char.
char :: P Char
char x =
  maybe (Left "Empty input") (return . fst) . T.uncons =<< text x

-- |
-- Any of the variable-length character types:
-- BPCHAR, VARCHAR, NAME and TEXT.
text :: P Text
text =
  either (Left . fromString . show) Right . TE.decodeUtf8'

{-# INLINE bytea #-}
bytea :: P ByteString
bytea =
  Right

-- * Date and Time
-------------------------

date :: P Day
date =
  fmap (Date.postgresJulianToDay . fromIntegral) . (int :: P Int32)

time :: P TimeOfDay
time =
  fmap (timeToTimeOfDay . picosecondsToDiffTime . (* (10^6)) . fromIntegral) . 
  (int :: P Word64)

-- * Misc
-------------------------

bool :: P Bool
bool b =
  case B.uncons b of
    Just (0, _) -> return False
    Just (1, _) -> return True
    _ -> Left ("Invalid value: " <> (fromString . show) b)

-- |
-- Arbitrary array.
-- 
-- Returns an intermediate representation,
-- which can then be used to decode into a specific data type.
{-# INLINE array #-}
array :: P Array.Data
array =
  flip Atto.run Atto.array

module PostgreSQLBinary.Decoder where

import PostgreSQLBinary.Prelude hiding (bool)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified PostgreSQLBinary.Decoder.Atto as Atto
import qualified PostgreSQLBinary.Array as Array
import qualified PostgreSQLBinary.Date as Date
import qualified PostgreSQLBinary.Integral as Integral
import qualified PostgreSQLBinary.Numeric as Numeric


-- |
-- A function for decoding a byte string into a value.
type D a = ByteString -> Either Text a


-- * Numbers
-------------------------

-- |
-- Any of PostgreSQL integer types.
{-# INLINE int #-}
int :: (Integral a, Bits a) => D a
int =
  Right . Integral.pack

float4 :: D Float
float4 =
  unsafeCoerce . (int :: D Word32)

float8 :: D Double
float8 =
  unsafeCoerce . (int :: D Word64)

numeric :: D Scientific
numeric =
  join . fmap Numeric.toScientific . flip Atto.run Atto.numeric

-- * Text
-------------------------

-- |
-- A UTF-8-encoded char.
char :: D Char
char x =
  maybe (Left "Empty input") (return . fst) . T.uncons =<< text x

-- |
-- Any of the variable-length character types:
-- BPCHAR, VARCHAR, NAME and TEXT.
text :: D Text
text =
  either (Left . fromString . show) Right . TE.decodeUtf8'

{-# INLINE bytea #-}
bytea :: D ByteString
bytea =
  Right

-- * Date and Time
-------------------------

date :: D Day
date =
  fmap (Date.postgresJulianToDay . fromIntegral) . (int :: D Int32)

time :: D TimeOfDay
time =
  fmap (timeToTimeOfDay . picosecondsToDiffTime . (* (10^6)) . fromIntegral) . 
  (int :: D Word64)

timetz :: D (TimeOfDay, TimeZone)
timetz =
  \x -> 
    let (timeX, zoneX) = B.splitAt 8 x
        in (,) <$> time timeX <*> tz zoneX
  where
    tz =
      fmap (minutesToTimeZone . negate . (`div` 60) . fromIntegral) . (int :: D Int32)

timestamp :: D LocalTime
timestamp =
  fmap fromMicros . (int :: D Int64)
  where
    fromMicros =
      evalState $ do
        days <- state $ (`divMod` (10^6 * 60 * 60 * 24))
        micros <- get
        return $
          LocalTime 
            (Date.postgresJulianToDay . fromIntegral $ days)
            (timeToTimeOfDay . picosecondsToDiffTime . (* (10^6)) . fromIntegral $ micros)

-- * Misc
-------------------------

bool :: D Bool
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
array :: D Array.Data
array =
  flip Atto.run Atto.array

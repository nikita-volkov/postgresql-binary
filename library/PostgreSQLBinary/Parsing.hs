-- |
-- Final specialized but uncomposable parsers.
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
import qualified PostgreSQLBinary.ArrayData as ArrayData
import qualified PostgreSQLBinary.Date as Date
import qualified PostgreSQLBinary.Integral as Integral
import qualified PostgreSQLBinary.Numeric as Numeric


type P a = ByteString -> Either Text a

utf8Char :: P Char
utf8Char x =
  maybe (Left "Empty input") (return . fst) . T.uncons =<< text x 

bool :: P Bool
bool b =
  case B.uncons b of
    Just (0, _) -> return False
    Just (1, _) -> return True
    _ -> Left ("Invalid value: " <> (fromString . show) b)

{-# INLINE integral #-}
integral :: (Integral a, Bits a) => P a
integral =
  Right . Integral.pack

float :: P Float
float =
  unsafeCoerce . (integral :: P Word32)

double :: P Double
double =
  unsafeCoerce . (integral :: P Word64)

numeric :: P Numeric.Numeric
numeric =
  flip Atto.run Atto.numeric

scientific :: P Scientific
scientific =
  join . fmap Numeric.toScientific . numeric

{-# INLINE arrayData #-}
arrayData :: P ArrayData.Data
arrayData =
  flip Atto.run Atto.arrayData

day :: P Day
day =
  fmap (Date.postgresJulianToDay . fromIntegral) . (integral :: P Int32)

timeOfDay :: P TimeOfDay
timeOfDay =
  fmap (timeToTimeOfDay . picosecondsToDiffTime . (* (10^6)) . fromIntegral) . 
  (integral :: P Word64)

text :: P Text
text =
  either (Left . fromString . show) Right . TE.decodeUtf8'

lazyText :: P TL.Text
lazyText =
  fmap TL.fromStrict . text

{-# INLINE byteString #-}
byteString :: P ByteString
byteString =
  Right

{-# INLINE lazyByteString #-}
lazyByteString :: P BL.ByteString
lazyByteString =
  Right . BL.fromStrict

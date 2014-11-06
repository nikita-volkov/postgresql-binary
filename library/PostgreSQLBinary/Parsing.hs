-- |
-- Final specialized but uncomposable parsers.
module PostgreSQLBinary.Parsing where

import PostgreSQLBinary.Prelude hiding (bool)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified PostgreSQLBinary.Parsing.Atto as Atto
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified PostgreSQLBinary.Array as Array
import qualified PostgreSQLBinary.Time as Time
import qualified PostgreSQLBinary.Parsing.Numeric as Numeric


type P a = ByteString -> Either Text a

char :: P Char
char =
  $notImplemented

bool :: P Bool
bool b =
  case B.uncons b of
    Just (0, _) -> return False
    Just (1, _) -> return True
    _ -> Left ("Invalid value: " <> (fromString . show) b)

{-# INLINE integral #-}
integral :: (Integral a, Bits a) => P a
integral =
  Right . Numeric.pack

{-# INLINE arrayData #-}
arrayData :: P Array.Data
arrayData =
  flip Atto.run Atto.arrayData

day :: P Day
day =
  fmap (Time.postgresJulianToDay . fromIntegral) . flip Atto.run Atto.int32

text :: P Text
text =
  either (Left . fromString . show) Right . Data.Text.Encoding.decodeUtf8'

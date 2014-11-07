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
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified PostgreSQLBinary.ArrayData as ArrayData
import qualified PostgreSQLBinary.Time as Time
import qualified PostgreSQLBinary.Parsing.Numeric as Numeric


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
  Right . Numeric.pack

{-# INLINE arrayData #-}
arrayData :: P ArrayData.Data
arrayData =
  flip Atto.run Atto.arrayData

day :: P Day
day =
  fmap (Time.postgresJulianToDay . fromIntegral) . flip Atto.run Atto.int32

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

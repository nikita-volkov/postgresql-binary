-- |
-- Final specialized but uncomposable parsers.
module LibpqBinary.Parsing where

import LibpqBinary.Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified LibpqBinary.Parsing.Atto as Atto


class Parsable a where
  parse :: Maybe ByteString -> Either Text a


type P a = Maybe ByteString -> Either Text a

char :: P Char
char =
  $notImplemented

bool :: P Bool
bool b =
  case B.uncons =<< b of
    Just (0, _) -> return False
    Just (1, _) -> return True
    _ -> Left ("Invalid value: " <> (fromString . show) b)



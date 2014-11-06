-- |
-- Final specialized but uncomposable parsers.
module LibpqBinary.Parsing where

import LibpqBinary.Prelude hiding (bool)
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
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto


class Parsable a where
  parse :: Maybe ByteString -> Either Text a

instance Parsable Bool where
  parse = parseFromP bool

parseFromP :: P a -> Maybe ByteString -> Either Text a
parseFromP p =
  fromMaybe (Left "Unexpected NULL") . fmap p


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
integral b =
  Atto.run b Atto.integral



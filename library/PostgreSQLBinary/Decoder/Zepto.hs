-- |
-- Composable Attoparsec parsers.
module PostgreSQLBinary.Decoder.Zepto where

import PostgreSQLBinary.Prelude hiding (take, bool)
import Data.Attoparsec.Zepto
import qualified PostgreSQLBinary.Integral as Integral
import qualified PostgreSQLBinary.Array as Array
import qualified Data.Scientific as Scientific


run :: ByteString -> Parser a -> Either Text a
run input parser =
  either (Left . fromString . show) Right $ parse parser input    

endOfInput :: Parser ()
endOfInput =
  atEnd >>= \case True -> return (); False -> fail "Not an end of input"

{-# INLINE intOfSize #-}
intOfSize :: (Integral a, Bits a) => Int -> Parser a
intOfSize x =
  Integral.pack <$> take x


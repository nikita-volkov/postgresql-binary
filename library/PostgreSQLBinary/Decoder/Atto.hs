-- |
-- Composable Attoparsec parsers.
module PostgreSQLBinary.Decoder.Atto where

import PostgreSQLBinary.Prelude hiding (take, bool)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 hiding (double)
import qualified PostgreSQLBinary.Integral as Integral
import qualified PostgreSQLBinary.Array as Array
import qualified Data.Scientific as Scientific


{-# INLINABLE run #-}
run :: ByteString -> Parser a -> Either Text a
run input parser =
  onResult $ parse (parser <* endOfInput) input    
  where
    onResult =
      \case
        Fail remainder contexts message ->
          Left $ "Message: " <> (fromString . show) message <> "; " <>
                 "Contexts: " <> (fromString . show) contexts <> "; " <>
                 "Failing input: " <> (fromString . show) remainder
        Partial c ->
          onResult $ c mempty
        Done _ result ->
          Right result

{-# INLINE integral #-}
integral :: forall a. (Integral a, Bits a) => Parser a
integral =
  intOfSize (Integral.byteSize (undefined :: a))

{-# INLINE intOfSize #-}
intOfSize :: (Integral a, Bits a) => Int -> Parser a
intOfSize x =
  Integral.pack <$> take x

bool :: Parser Bool
bool =
  (word8 0 *> pure False) <|> (word8 1 *> pure True)

double :: Parser Double
double =
  unsafeCoerce (intOfSize 8 :: Parser Int64)

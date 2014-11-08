-- |
-- Composable Attoparsec parsers.
module PostgreSQLBinary.Parsing.Atto where

import PostgreSQLBinary.Prelude hiding (take, bool)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 hiding (double)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified PostgreSQLBinary.Integral as Integral
import qualified PostgreSQLBinary.ArrayData as ArrayData
import qualified PostgreSQLBinary.Numeric as Numeric


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
  sizedIntegral (Integral.byteSize (undefined :: a))

{-# INLINE sizedIntegral #-}
sizedIntegral :: (Integral a, Bits a) => Int -> Parser a
sizedIntegral x =
  Integral.pack <$> take x

word16 :: Parser Word16
word16 =
  sizedIntegral 2

word32 :: Parser Word32
word32 =
  sizedIntegral 4

int32 :: Parser Int32
int32 =
  sizedIntegral 4

bool :: Parser Bool
bool =
  (word8 0 *> pure False) <|> (word8 1 *> pure True)

arrayData :: Parser ArrayData.Data
arrayData =
  do
    dimensionsAmountV <- word32
    nullsV <- nulls
    oidV <- word32
    dimensionsV <- replicateM (fromIntegral dimensionsAmountV) dimension
    valuesV <- many value
    return (dimensionsV, valuesV, nullsV, oidV)
  where
    dimension =
      (,) <$> word32 <*> word32
    value =
      nothing <|> just
      where
        nothing =
          string (Integral.unpack (-1 :: Word32)) *> pure Nothing
        just =
          do
            length <- word32
            b <- take (fromIntegral length)
            return $ Just b
    nulls =
      word32 >>= \case
        0 -> return False
        1 -> return True
        w -> fail $ "Invalid value: " <> show w

numeric :: Parser Numeric.Numeric
numeric =
  do
    componentsAmount <- sizedIntegral 2
    weight <- sizedIntegral 2
    signCode <- sizedIntegral 2
    digitsAfterPoint <- sizedIntegral 2
    components <- replicateM (fromIntegral componentsAmount) (sizedIntegral 2)
    return $ Numeric.Numeric componentsAmount weight signCode digitsAfterPoint components

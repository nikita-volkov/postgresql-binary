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
import qualified PostgreSQLBinary.Array as Array
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

array :: Parser Array.Data
array =
  do
    dimensionsAmountV <- intOfSize 4
    nullsV <- nulls
    oidV <- intOfSize 4
    dimensionsV <- replicateM dimensionsAmountV dimension
    valuesV <- many value
    return (dimensionsV, valuesV, nullsV, oidV)
  where
    dimension =
      (,) <$> intOfSize 4 <*> intOfSize 4
    value =
      nothing <|> just
      where
        nothing =
          string (Integral.unpack (-1 :: Word32)) *> pure Nothing
        just =
          Just <$> (take =<< intOfSize 4)
    nulls =
      (intOfSize 4 :: Parser Word32) >>= \case
        0 -> return False
        1 -> return True
        w -> fail $ "Invalid value: " <> show w

numeric :: Parser Numeric.Numeric
numeric =
  do
    componentsAmount <- intOfSize 2
    weight <- intOfSize 2
    signCode <- intOfSize 2
    digitsAfterPoint <- intOfSize 2
    components <- replicateM (fromIntegral componentsAmount) (intOfSize 2)
    return $ Numeric.Numeric componentsAmount weight signCode digitsAfterPoint components

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
import qualified Data.Scientific as Scientific


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

scientific :: Parser Scientific
scientific =
  do
    -- Length of a components list:
    componentsAmount <- sizedIntegral 2
    -- Index of the point in the components list:
    weight <- take 2
    -- A function, which signs a number:
    signer <- do
      -- Code of a sign:
      code <- sizedIntegral 2
      if 
        | code == negSignCode -> return negate
        | code == posSignCode -> return id
        | code == nanSignCode -> fail "NAN sign"
        | otherwise           -> fail $ "Unexpected sign value: " <> show code
    -- Amount of digits after the point:
    digitsAfterPoint <- sizedIntegral 2
    components <- replicateM componentsAmount (sizedIntegral 2)
    let
      c = signer $ decodeComponents components
      e = negate $ digitsAfterPoint
      in 
        return $ Scientific.scientific c e
  where
    posSignCode = 0x0000 :: Word16
    negSignCode = 0x4000 :: Word16
    nanSignCode = 0xC000 :: Word16
    decodeComponents = 
      foldl' (\l r -> l * 10 + r) 0 . foldMap unpackComponent
    unpackComponent =
      evalState $ do
        a <- state (`divMod` 1000)
        b <- state (`divMod` 100)
        c <- state (`divMod` 10)
        d <- get
        return $ [a, b, c, d]

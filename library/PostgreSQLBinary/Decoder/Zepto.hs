-- |
-- Composable Attoparsec parsers.
module PostgreSQLBinary.Decoder.Zepto where

import PostgreSQLBinary.Prelude hiding (take, bool)
import Data.Attoparsec.Zepto
import qualified PostgreSQLBinary.Integral as Integral
import qualified PostgreSQLBinary.Array as Array
import qualified PostgreSQLBinary.Numeric as Numeric
import qualified Data.Scientific as Scientific


{-# INLINE run #-}
run :: ByteString -> Parser a -> Either Text a
run input parser =
  either (Left . fromString . show) Right $ parse parser input    

{-# INLINE endOfInput #-}
endOfInput :: Parser ()
endOfInput =
  atEnd >>= \case True -> return (); False -> fail "Not an end of input"

{-# INLINE intOfSize #-}
intOfSize :: (Integral a, Bits a) => Int -> Parser a
intOfSize x =
  Integral.pack <$> take x

{-# INLINABLE array #-}
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

{-# INLINE numeric #-}
numeric :: Parser Scientific
numeric =
  do
    componentsAmount <- intOfSize 2
    pointIndex :: Int16 <- intOfSize 2
    signCode <- intOfSize 2
    take 2
    components <- replicateM componentsAmount (intOfSize 2)
    signer <-
      if | signCode == Numeric.negSignCode -> return negate
         | signCode == Numeric.posSignCode -> return id
         | signCode == Numeric.nanSignCode -> fail "NAN sign"
         | otherwise -> fail $ "Unexpected sign value: " <> show signCode
    let
      c = signer $ fromIntegral $ (Numeric.mergeComponents components :: Word64)
      e = (fromIntegral (pointIndex + 1) - length components) * 4
      in return $ Scientific.scientific c e

-- |
-- Composable Attoparsec parsers.
module PostgreSQLBinary.Decoder.Zepto where

import PostgreSQLBinary.Prelude hiding (take, bool)
import Data.Attoparsec.Zepto
import qualified PostgreSQLBinary.Integral as Integral
import qualified PostgreSQLBinary.Numeric as Numeric
import qualified Data.Scientific as Scientific


{-# INLINE run #-}
run :: ByteString -> Parser a -> Either Text a
run input parser =
  either (Left . fromString . show) Right $ parse parser input    

{-# INLINE intOfSize #-}
intOfSize :: (Integral a, Bits a) => Int -> Parser a
intOfSize x =
  Integral.pack <$> take x

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

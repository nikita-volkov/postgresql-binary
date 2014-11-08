module PostgreSQLBinary.Numeric where

import PostgreSQLBinary.Prelude
import qualified Data.Scientific as Scientific


-- |
-- An intermediate representation of a numeric postgres data type.
-- 
-- Maps directly to the binary protocol.
data Numeric = Numeric {
  -- | A length of the components list
  componentsAmount :: Word16,
  -- | An index of the point in the components list
  pointIndex :: Int16,
  -- | A code of the sign
  signCode :: Word16,
  -- | An amount of digits after the point
  amountOfDigitsAfterPoint :: Word16,
  -- | Components of the number
  components :: [Word16]
} deriving (Show)


posSignCode = 0x0000 :: Word16
negSignCode = 0x4000 :: Word16
nanSignCode = 0xC000 :: Word16

extractComponents :: Integral a => a -> [Word16]
extractComponents =
  (reverse .) . (. abs) . unfoldr $ \case
    0 -> Nothing
    x -> case divMod x 10000 of
      (d, m) -> Just (fromIntegral m, d)

mergeComponents :: Integral a => [Word16] -> a
mergeComponents = 
  foldl' (\l r -> l * 10000 + fromIntegral r) 0

mergeDigits :: Integral a => [Word16] -> a
mergeDigits = 
  foldl' (\l r -> l * 10 + fromIntegral r) 0

-- |
-- Unpack a component into digits.
componentDigits :: Word16 -> [Word16]
componentDigits =
  evalState $ do
    a <- state (`divMod` 1000)
    b <- state (`divMod` 100)
    c <- state (`divMod` 10)
    d <- get
    return $ [a, b, c, d]

toScientific :: Numeric -> Either Text Scientific
toScientific x =
  do
    -- A function, which signs a number:
    signer <-
      if 
        | (signCode x) == negSignCode -> return negate
        | (signCode x) == posSignCode -> return id
        | (signCode x) == nanSignCode -> Left "NAN sign"
        | otherwise                   -> Left $ "Unexpected sign value: " <> 
                                                (fromString . show) (signCode x)
    let
      c = 
        signer $ mergeComponents (components x)
      e = 
        ((fromIntegral (pointIndex x) + 1) - length (components x)) * 4
      in 
        return $ Scientific.scientific c e

fromScientific :: Scientific -> Numeric
fromScientific x =
  Numeric (fromIntegral componentsAmount) (fromIntegral pointIndex) signCode (fromIntegral trimmedExponent) components
  where
    normalized =
      Scientific.normalize x
    componentsAmount = 
      length components
    coefficient =
      Scientific.coefficient normalized
    exponent = 
      Scientific.base10Exponent normalized
    components = 
      extractComponents tunedCoefficient
    pointIndex =
      componentsAmount + (tunedExponent `div` 4) - 1
    (tunedCoefficient, tunedExponent) =
      case mod exponent 4 of
        0 -> (coefficient, exponent)
        x -> (coefficient * 10 ^ x, exponent - x)
    trimmedExponent =
      if tunedExponent >= 0
        then 0
        else negate tunedExponent
    signCode =
      if coefficient < 0
        then negSignCode
        else posSignCode

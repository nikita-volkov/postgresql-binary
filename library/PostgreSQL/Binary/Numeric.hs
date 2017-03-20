module PostgreSQL.Binary.Numeric where

import PostgreSQL.Binary.Prelude
import qualified Data.Vector as Vector
import qualified Data.Scientific as Scientific


{-# INLINE posSignCode #-}
posSignCode :: Word16
posSignCode = 0x0000

{-# INLINE negSignCode #-}
negSignCode :: Word16
negSignCode = 0x4000

{-# INLINE nanSignCode #-}
nanSignCode :: Word16
nanSignCode = 0xC000

{-# INLINE extractComponents #-}
extractComponents :: Integral a => a -> [Word16]
extractComponents =
  (reverse .) . (. abs) . unfoldr $ \case
    0 -> Nothing
    x -> case divMod x 10000 of
      (d, m) -> Just (fromIntegral m, d)

{-# INLINE mergeComponents #-}
mergeComponents :: Integral a => Vector a -> Integer
mergeComponents = 
  Vector.foldl' (\l r -> l * 10000 + fromIntegral r) 0

{-# INLINE mergeDigits #-}
mergeDigits :: Integral a => Vector a -> a
mergeDigits = 
  Vector.foldl' (\l r -> l * 10 + r) 0

-- |
-- Unpack a component into digits.
{-# INLINE componentDigits #-}
componentDigits :: Int16 -> [Int16]
componentDigits =
  evalState $ do
    a <- state (flip divMod 1000)
    b <- state (flip divMod 100)
    c <- state (flip divMod 10)
    d <- get
    return $ [a, b, c, d]

{-# INLINABLE componentsReplicateM #-}
componentsReplicateM :: (Integral a, Applicative m) => Int -> m a -> m a
componentsReplicateM amount component = 
  foldl' folder (pure 0) (replicate amount component)
  where
    folder acc component =
      liftA2 (+) (fmap (*10000) acc) component

{-# INLINE signer #-}
signer :: Integral a => Word16 -> Either Text (a -> a)
signer =
  \case
    0x0000 -> return id
    0x4000 -> return negate
    0xC000 -> Left "NAN sign"
    signCode -> Left ("Unexpected sign code: " <> (fromString . show) signCode)

{-# INLINE scientific #-}
scientific :: Int16 -> Word16 -> Vector Word16 -> Either Text Scientific
scientific pointIndex signCode components =
  do
    theSigner <- signer signCode
    return (Scientific.scientific (c theSigner) e)
  where
    c signer =
      signer (mergeComponents components)
    e =
      (fromIntegral pointIndex + 1 - Vector.length components) * 4

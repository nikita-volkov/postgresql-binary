module PostgreSQLBinary.Numeric where

import PostgreSQLBinary.Prelude


posSignCode :: Word16 = 0x0000
negSignCode :: Word16 = 0x4000
nanSignCode :: Word16 = 0xC000

{-# INLINE extractComponents #-}
extractComponents :: Integral a => a -> [Int16]
extractComponents =
  (reverse .) . (. abs) . unfoldr $ \case
    0 -> Nothing
    x -> case divMod x 10000 of
      (d, m) -> Just (fromIntegral m, d)

{-# INLINE mergeComponents #-}
mergeComponents :: Integral a => [a] -> a
mergeComponents = 
  foldl' (\l r -> l * 10000 + r) 0

{-# INLINE mergeDigits #-}
mergeDigits :: Integral a => [a] -> a
mergeDigits = 
  foldl' (\l r -> l * 10 + r) 0

-- |
-- Unpack a component into digits.
{-# INLINE componentDigits #-}
componentDigits :: Int16 -> [Int16]
componentDigits =
  evalState $ do
    a <- state (`divMod` 1000)
    b <- state (`divMod` 100)
    c <- state (`divMod` 10)
    d <- get
    return $ [a, b, c, d]

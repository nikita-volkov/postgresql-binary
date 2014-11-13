module PostgreSQLBinary.Numeric where

import PostgreSQLBinary.Prelude


posSignCode = 0x0000 :: Word16
negSignCode = 0x4000 :: Word16
nanSignCode = 0xC000 :: Word16

{-# INLINE mergeComponents #-}
mergeComponents :: Integral a => [a] -> a
mergeComponents = 
  foldl' (\l r -> l * 10000 + r) 0

module PostgreSQL.Binary.BuilderPrim where

import PostgreSQL.Binary.Prelude
import qualified Data.ByteString.Builder.Prim as A


{-# INLINE nullByteIgnoringBoundedPrim #-}
nullByteIgnoringBoundedPrim :: A.BoundedPrim Word8
nullByteIgnoringBoundedPrim =
  A.condB (== 0) A.emptyB (A.liftFixedToBounded A.word8)

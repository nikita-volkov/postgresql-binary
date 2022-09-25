module PostgreSQL.Binary.BuilderPrim where

import qualified Data.ByteString.Builder.Prim as A
import PostgreSQL.Binary.Prelude

{-# INLINE nullByteIgnoringBoundedPrim #-}
nullByteIgnoringBoundedPrim :: A.BoundedPrim Word8
nullByteIgnoringBoundedPrim =
  A.condB (== 0) A.emptyB (A.liftFixedToBounded A.word8)

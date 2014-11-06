-- |
-- Composable Attoparsec parsers.
module LibpqBinary.Parsing.Atto where

import LibpqBinary.Prelude hiding (take, bool)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 hiding (double)
import qualified Data.ByteString
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified LibpqBinary.Parsing.Numeric as Numeric


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
  Numeric.pack <$> take (Numeric.byteSize (undefined :: a))

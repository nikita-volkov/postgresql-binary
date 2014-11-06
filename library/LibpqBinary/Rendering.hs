module LibpqBinary.Rendering where

import LibpqBinary.Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector as Vector
import qualified Data.ByteString.Builder.Scientific as Scientific


type R a = a -> Maybe ByteString

bool :: R Bool
bool =
  \case
    False -> return $ B.singleton 0
    True  -> return $ B.singleton 1

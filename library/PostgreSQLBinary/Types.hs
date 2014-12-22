module PostgreSQLBinary.Types 
(
  PGEnum(..)
)
where

-- base-prelude
-------------------------
import BasePrelude

-- text
-------------------------
import qualified Data.Text

newtype PGEnum = PGEnum {getEnumText :: Data.Text.Text}
                 deriving (Eq, Show)

  

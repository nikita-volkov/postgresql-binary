module Main where

import BasePrelude
import Criterion
import Criterion.Main
import qualified PostgreSQLBinary.Decoder as D


main =
  defaultMain
    [
      b "numeric"     D.numeric            "\NUL\EOT\NUL\NUL\NUL\NUL\NUL\f\NUL\DC4\bT#:\v\184"
    ]
  where
    b name decoder value = 
      bench name $ nf decoder value

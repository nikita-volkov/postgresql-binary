module PostgreSQL.Binary.Interval where

import PostgreSQL.Binary.Prelude hiding (months)
import qualified PostgreSQL.Binary.Time as Time

data Interval = Interval
  { micros :: Int64,
    days :: Int32,
    months :: Int32
  }
  deriving (Show, Eq)

-- |
-- Oddly enough despite a claim of support of up to 178000000 years in
-- <http://www.postgresql.org/docs/9.3/static/datatype-datetime.html Postgres' docs>
-- in practice it starts behaving unpredictably after a smaller limit.
maxDiffTime :: DiffTime
maxDiffTime = 1780000 * Time.microsToDiffTime Time.yearMicros

minDiffTime :: DiffTime
minDiffTime = negate maxDiffTime

fromDiffTime :: DiffTime -> Maybe Interval
fromDiffTime x =
  if x > maxDiffTime || x < minDiffTime
    then Nothing
    else Just $ fromPicosUnsafe (unsafeCoerce x)

fromPicosUnsafe :: Integer -> Interval
fromPicosUnsafe =
  evalState $ do
    modify $ flip div (10 ^ 6)
    u <- state $ swap . flip divMod (10 ^ 6 * 60 * 60 * 24)
    d <- state $ swap . flip divMod (31)
    m <- get
    return $ Interval (fromIntegral u) (fromIntegral d) (fromIntegral m)

toDiffTime :: Interval -> DiffTime
toDiffTime x =
  picosecondsToDiffTime
    $ (10 ^ 6)
    * ( fromIntegral (micros x)
          + 10
          ^ 6
          * 60
          * 60
          * 24
          * (fromIntegral (days x + 31 * months x))
      )

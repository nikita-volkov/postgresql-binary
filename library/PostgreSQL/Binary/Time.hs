module PostgreSQL.Binary.Time where

import PostgreSQL.Binary.Prelude hiding (second)

{-# INLINEABLE dayToPostgresJulian #-}
dayToPostgresJulian :: Day -> Integer
dayToPostgresJulian =
  (+ (2400001 - 2451545)) . toModifiedJulianDay

{-# INLINEABLE postgresJulianToDay #-}
postgresJulianToDay :: (Integral a) => a -> Day
postgresJulianToDay =
  ModifiedJulianDay . fromIntegral . subtract (2400001 - 2451545)

{-# INLINEABLE microsToTimeOfDay #-}
microsToTimeOfDay :: Int64 -> TimeOfDay
microsToTimeOfDay =
  evalState $ do
    h <- state $ flip divMod $ 10 ^ 6 * 60 * 60
    m <- state $ flip divMod $ 10 ^ 6 * 60
    u <- get
    return
      $ TimeOfDay (fromIntegral h) (fromIntegral m) (microsToPico u)

{-# INLINEABLE microsToUTC #-}
microsToUTC :: Int64 -> UTCTime
microsToUTC =
  evalState $ do
    d <- state $ flip divMod $ 10 ^ 6 * 60 * 60 * 24
    u <- get
    return
      $ UTCTime (postgresJulianToDay d) (microsToDiffTime u)

{-# INLINEABLE microsToPico #-}
microsToPico :: Int64 -> Pico
microsToPico =
  unsafeCoerce . (* (10 ^ 6)) . (fromIntegral :: Int64 -> Integer)

{-# INLINEABLE microsToDiffTime #-}
microsToDiffTime :: Int64 -> DiffTime
microsToDiffTime =
  unsafeCoerce microsToPico

{-# INLINEABLE microsToLocalTime #-}
microsToLocalTime :: Int64 -> LocalTime
microsToLocalTime =
  evalState $ do
    d <- state $ flip divMod $ 10 ^ 6 * 60 * 60 * 24
    u <- get
    return
      $ LocalTime (postgresJulianToDay d) (microsToTimeOfDay u)

{-# INLINEABLE secsToTimeOfDay #-}
secsToTimeOfDay :: Double -> TimeOfDay
secsToTimeOfDay =
  evalState $ do
    h <- state $ flip divMod' $ 60 * 60
    m <- state $ flip divMod' $ 60
    s <- get
    return
      $ TimeOfDay (fromIntegral h) (fromIntegral m) (secsToPico s)

{-# INLINEABLE secsToUTC #-}
secsToUTC :: Double -> UTCTime
secsToUTC =
  evalState $ do
    d <- state $ flip divMod' $ 60 * 60 * 24
    s <- get
    return
      $ UTCTime (postgresJulianToDay d) (secsToDiffTime s)

{-# INLINEABLE secsToLocalTime #-}
secsToLocalTime :: Double -> LocalTime
secsToLocalTime =
  evalState $ do
    d <- state $ flip divMod' $ 60 * 60 * 24
    s <- get
    return
      $ LocalTime (postgresJulianToDay d) (secsToTimeOfDay s)

{-# INLINEABLE secsToPico #-}
secsToPico :: Double -> Pico
secsToPico s =
  unsafeCoerce (truncate $ toRational s * 10 ^ 12 :: Integer)

{-# INLINEABLE secsToDiffTime #-}
secsToDiffTime :: Double -> DiffTime
secsToDiffTime =
  unsafeCoerce secsToPico

{-# INLINEABLE localTimeToMicros #-}
localTimeToMicros :: LocalTime -> Int64
localTimeToMicros (LocalTime dayX timeX) =
  let d = dayToPostgresJulian dayX
      p = unsafeCoerce $ timeOfDayToTime timeX
   in 10 ^ 6 * 60 * 60 * 24 * fromIntegral d + fromIntegral (div p (10 ^ 6))

{-# INLINEABLE localTimeToSecs #-}
localTimeToSecs :: LocalTime -> Double
localTimeToSecs (LocalTime dayX timeX) =
  let d = dayToPostgresJulian dayX
      p = unsafeCoerce $ timeOfDayToTime timeX
   in 60 * 60 * 24 * fromIntegral d + fromRational (p % (10 ^ 12))

{-# INLINEABLE utcToMicros #-}
utcToMicros :: UTCTime -> Int64
utcToMicros (UTCTime dayX diffTimeX) =
  let d = dayToPostgresJulian dayX
      p = unsafeCoerce diffTimeX
   in 10 ^ 6 * 60 * 60 * 24 * fromIntegral d + fromIntegral (div p (10 ^ 6))

{-# INLINEABLE utcToSecs #-}
utcToSecs :: UTCTime -> Double
utcToSecs (UTCTime dayX diffTimeX) =
  let d = dayToPostgresJulian dayX
      p = unsafeCoerce diffTimeX
   in 60 * 60 * 24 * fromIntegral d + fromRational (p % (10 ^ 12))

-- * Constants in microseconds according to Julian dates standard

-- According to
-- http://www.postgresql.org/docs/9.1/static/datatype-datetime.html
-- Postgres uses Julian dates internally

yearMicros :: Int64
yearMicros = truncate (365.2425 * fromIntegral dayMicros :: Rational)

dayMicros :: Int64
dayMicros = 24 * hourMicros

hourMicros :: Int64
hourMicros = 60 * minuteMicros

minuteMicros :: Int64
minuteMicros = 60 * secondMicros

secondMicros :: Int64
secondMicros = 10 ^ 6
